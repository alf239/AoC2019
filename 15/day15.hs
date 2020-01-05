module Main where

import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State.Strict
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.List.Split
import           Data.Sequence                  ( Seq
                                                , viewl
                                                , ViewL(..)
                                                , (><)
                                                )
import qualified Data.Sequence                 as Seq
import           Intcode
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           System.Console.ANSI
import           Debug.Trace

data Dir    = North | South | West | East deriving (Show, Eq, Enum, Bounded)
data Loc    = Space | Wall | Oxy  deriving (Show, Eq, Enum)
type Coord = (Int, Int)

data ExplorerS = ExplorerS {
    explored :: Map Coord Loc,
    droid    :: Coord,
    plan     :: [Dir],
    bfs      :: Seq Coord
} deriving (Show)

type Explorer = StateT ExplorerS IO

command :: Dir -> Value
command = (+ 1) . fromIntegral . fromEnum

block :: Loc -> Char
block Space = '.'
block Wall  = '#'
block Oxy   = 'o'

move :: Dir -> Coord -> Coord
move North (x, y) = (x, y - 1)
move East  (x, y) = (x + 1, y)
move South (x, y) = (x, y + 1)
move West  (x, y) = (x - 1, y)

options :: Map Coord Loc -> Coord -> [Coord]
options m d =
    filter (\k -> Map.notMember k m)
        . map (\dir -> move dir d)
        $ [minBound .. maxBound]

route :: Map Coord Loc -> Coord -> Coord -> [Dir]
route m s g = route' (Seq.singleton (s, [])) Set.empty
  where
    route' q seen =
        let ((x, p) :< xs) = viewl q
        in
            if x == g
                then reverse p
                else if (Set.member x seen || Map.lookup x m /= Just Space)
                    then route' xs seen
                    else
                        let
                            options = map (\d -> (move d x, d : p))
                                          [minBound .. maxBound]
                        in  route' (xs >< Seq.fromList options)
                                   (Set.insert x seen)

dequeueGoal :: Explorer Coord
dequeueGoal = do
    seen <- gets explored
    iterateWhile (\x -> Map.member x seen) $ do
        x :< xs <- gets $ viewl . bfs
        modify $ \s -> s { bfs = xs }
        return x

planNextRoute :: Explorer ()
planNextRoute = do
    next      <- dequeueGoal
    explored' <- gets explored
    droid'    <- gets droid
    let path' = route explored' droid' next
    modify $ \s -> s { plan = path' }


readE :: Explorer Value
readE = do
    p <- gets plan
    return $ case p of
        []      -> 0
        (x : _) -> command x

writeE :: Value -> Explorer ()
writeE 0 = do -- hit the wall
    (d : ds) <- gets plan
    if null ds
        then do
            explored' <- gets explored
            droid'    <- gets droid
            modify $ \s ->
                s { explored = Map.insert (move d droid') Wall explored' }
            planNextRoute
        else error "We shouldn't hit walls en route"
writeE 1 = do -- stepped just fine
    (d : ds) <- gets plan
    modify $ \s -> s { droid = move d (droid s) }
    if null ds
        then do
            explored' <- gets explored
            droid'    <- gets droid
            let explored'' = Map.insert droid' Space explored'
            modify $ \s -> s { explored = explored'' }
            modify
                $ \s -> s
                      { bfs = bfs s
                                  >< (Seq.fromList $ options explored'' droid')
                      }
            planNextRoute
        else modify $ \s -> s { plan = ds }
writeE 2 = do -- found the valve
    d         <- gets $ head . plan
    explored' <- gets explored
    droid'    <- gets $ move d . droid
    modify $ \s -> s { droid = droid', plan = [] }

runDroid :: [Value] -> IO ExplorerS
runDroid m =
    let intCode    = execIntcodeT readE writeE m
        initialMap = Map.singleton (0, 0) Space
        ioProgram  = execStateT intCode $ ExplorerS
            { explored = initialMap
            , droid    = (0, 0)
            , plan     = [North]
            , bfs      = Seq.fromList $ options initialMap (0, 0)
            }
    in  ioProgram

main :: IO ()
main = do
    code       <- map read . splitOn "," <$> getContents
    fs <- runDroid code
    let rt = route (explored fs) (0, 0) (droid fs)
    print $ length rt

