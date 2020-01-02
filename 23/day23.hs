module Main where 

import Intcode
import Data.List.Extra (groupOn)
import Data.List.Split (splitOn, chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent (forkIO, yield)
import Control.Concurrent.Chan.Unagi.NoBlocking
import Control.Exception (handle)
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State.Strict
import Debug.Trace
import System.IO

type InMessage = (Value, Value)
type OutMessage = Either Value (Value, Value, Value)
type ChannelIo = StateT (Bool, Maybe Value, Stream InMessage, Out) IO
                           
readCh :: Int -> InChan OutMessage -> ChannelIo Value
readCh id o = do (ready, ib, str, ob) <- get
                 if ready then case ib of
                                    (Just x) -> do put (True, Nothing, str, ob)
                                                   return x
                                    Nothing  -> do msg <- liftIO . tryReadNext $ str
                                                   case msg of
                                                       Next (a, b) str' -> do put (True, Just b, str', ob)
                                                                              return a
                                                       Pending          -> do liftIO . writeChan o $ Left (fromIntegral id)
                                                                              liftIO yield
                                                                              return $ negate 1
                          else do put (True, ib, str, ob)
                                  return (fromIntegral id)

writeCh :: InChan OutMessage -> Value -> ChannelIo ()
writeCh ch a = do (_, _, _, buffer) <- get
                  case buffer of
                    [b, c]    -> do liftIO . writeChan ch $ Right (c, b, a)
                                    liftIO yield
                                    modify $ \(i, ib, str, _) -> (i, ib, str, [])
                    otherwise -> modify $ \(i, ib, str, _) -> (i, ib, str, a : buffer)

runCh :: IntState -> Int -> OutChan InMessage -> InChan OutMessage -> IO ()
runCh ic id i o = let rd = readCh id o
                      wr = writeCh o
                      state = runStateT (runProgram rd wr) ic
                  in do [str] <- streamChan 1 i
                        result <- evalStateT state (False, Nothing, str, [])
                        return $ fst result


consume :: Stream OutMessage -> [InChan InMessage] -> StateT (InMessage, Set Value) IO ()
consume str cs  = do msg <- liftIO . tryReadNext $ str
                     case msg of 
                       Pending -> do (xy, stale) <- get
                                     when (Set.size stale == length cs) $ do put (xy, Set.empty)
                                                                             liftIO . writeChan (head cs) $ xy
                                                                             liftIO . print $ ("Stalling", xy)
                                     liftIO yield
                                     consume str cs
                       Next (Right (255, x, y)) str' -> do modify $ \(_, stale) -> ((x, y), stale)
                                                           liftIO . print $ ("NAT update", (x, y))
                                                           consume str' cs
                       Next (Right (id, x, y)) str' -> do modify $ \(xy, stale) -> (xy, Set.delete id stale)
                                                          liftIO . writeChan (cs !! fromIntegral id) $ (x, y)
                                                          consume str' cs
                       Next (Left id) str'  -> do modify $ \(xy, stale) -> (xy, Set.insert id stale)
                                                  consume str' cs

coordinate :: OutChan OutMessage -> [InChan InMessage] -> StateT (InMessage, Set Value) IO ()
coordinate ch cs = do [str] <- liftIO . streamChan 1 $ ch
                      consume str cs


main :: IO ()
main = do baseNic <- fromMemory . map read . splitOn "," <$> getContents
          (ci, co) <- newChan

          cs <- forM [0..49] $ \id ->
                do (i, o) <- newChan
                   _ <- forkIO $ runCh baseNic id o ci
                   return i

          hSetBuffering stdout LineBuffering

          evalStateT (coordinate co cs) ((-1, -1), Set.empty)