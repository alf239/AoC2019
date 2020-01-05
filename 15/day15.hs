module Main where

import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State.Strict
import           Data.Functor.Identity
import           Data.List.Split
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Sequence                  ( Seq
                                                , viewl
                                                , ViewL(..)
                                                , (><)
                                                )
import qualified Data.Sequence                 as Seq
import           Intcode

data Dir    = North | South | West | East deriving (Show, Eq, Enum, Bounded)
data Loc    = Space | Wall | Oxy  deriving (Show, Eq, Enum)
type Coord = (Int, Int)

data ExplorerS = ExplorerS {
    explored :: Map Coord Loc,
    droid    :: Coord,
    plan     :: [Dir],
    bfs      :: Seq Coord,
    oxygen   :: Maybe Coord
} deriving (Show)

type Explorer = StateT ExplorerS Maybe

command :: Dir -> Value
command = (+ 1) . fromIntegral . fromEnum

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

navigable :: Map Coord Loc -> Coord -> Bool
navigable m x = case Map.lookup x m of
    Just Space -> True
    Just Oxy   -> True
    otherwise  -> False

neighbours :: Coord -> (Dir -> a) -> Seq (Coord, a)
neighbours x f =
    Seq.fromList $ map (\d -> (move d x, f d)) [minBound .. maxBound]

route :: Map Coord Loc -> Coord -> Coord -> [Dir]
route m s g = route' (Seq.singleton (s, [])) Set.empty
  where
    route' q seen = case viewl q of
        EmptyL -> error ("no path found from " ++ show s ++ " to " ++ show g)
        ((x, p) :< xs)
            | x == g -> reverse p
            | Set.member x seen || not (navigable m x) -> route' xs seen
            | otherwise -> route' (xs >< neighbours x (\d -> d : p))
                                  (Set.insert x seen)

diameter :: Map Coord Loc -> Coord -> Int
diameter m s = route' (Seq.singleton (s, 0)) Set.empty 0
  where
    route' q seen a = case viewl q of
        EmptyL -> a
        ((x, d) :< xs)
            | Set.member x seen || not (navigable m x) -> route' xs seen a
            | otherwise -> route' (xs >< neighbours x (const $ d + 1))
                                  (Set.insert x seen)
                                  d

dequeueGoal :: Explorer (Maybe Coord)
dequeueGoal = do
    seen <- gets explored
    iterateWhile (already seen) $ do
        queue <- gets $ viewl . bfs
        case queue of
            x :< xs -> do
                modify $ \s -> s { bfs = xs }
                return $ Just x
            EmptyL -> return Nothing
  where
    already _    Nothing  = False
    already seen (Just x) = Map.member x seen

planNextRoute :: Explorer ()
planNextRoute = do
    next <- dequeueGoal
    case next of
        Nothing    -> modify $ \s -> s { plan = [] } -- Request stop
        Just next' -> do
            explored' <- gets explored
            droid'    <- gets droid
            let path' = route explored' droid' next'
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
writeE x = do -- stepped just fine, perhaps found the oxygen valve
    (d : ds) <- gets plan
    droid'   <- gets $ move d . droid
    modify $ \s -> s { droid = droid' }
    if x == 2 then modify $ \s -> s { oxygen = Just droid' } else return ()
    if null ds
        then do
            explored' <- gets explored
            let explored'' =
                    Map.insert droid' (if x == 1 then Space else Oxy) explored'
            modify $ \s -> s { explored = explored'' }
            modify
                $ \s -> s
                      { bfs = bfs s
                                  >< (Seq.fromList $ options explored'' droid')
                      }
            planNextRoute
        else modify $ \s -> s { plan = ds }

runDroid :: [Value] -> Maybe ExplorerS
runDroid m =
    let intCode    = execIntcodeT readE writeE m
        initialMap = Map.singleton (0, 0) Space
        program    = execStateT intCode $ ExplorerS
            { explored = initialMap
            , droid    = (0, 0)
            , plan     = [North]
            , bfs      = Seq.fromList $ options initialMap (0, 0)
            , oxygen   = Nothing
            }
    in  program

main :: IO ()
main = do
    code <- map read . splitOn "," <$> getContents
    let Just fs  = runDroid code
    let fullMap  = explored fs
    let Just oxy = oxygen fs
    let rt       = route fullMap (0, 0) oxy
    putStrLn "=== Task 1 ==="
    print $ length rt

    putStrLn "=== Task 2 ==="
    print $ diameter fullMap oxy
