module Main where

import Debug.Trace

data Pos = Pos Int Int Int deriving (Show, Eq)
data Vel = Vel Int Int Int deriving (Show, Eq)

data Body = Body Pos Vel deriving (Show, Eq)

gravity :: [Body] -> [Body]
gravity xs = [gravity' a xs | a <- xs]
             where gravity' a bs = let Body (Pos x y z) (Vel dx dy dz) = a
                                    in Body (Pos x y z) (Vel (dx + sum [signum (x1 - x) | Body (Pos x1 _ _) _ <- bs]) 
                                                             (dy + sum [signum (y1 - y) | Body (Pos _ y1 _) _ <- bs]) 
                                                             (dz + sum [signum (z1 - z) | Body (Pos _ _ z1) _ <- bs]))

move :: [Body] -> [Body]
move = map move1 where move1 (Body (Pos x y z) (Vel dx dy dz)) = Body (Pos (x + dx) (y + dy) (z + dz)) (Vel dx dy dz)

step :: [Body] -> [Body]
step xs = move . gravity $! xs

potential (Body (Pos x y z) _) = (abs x) + (abs y) + (abs z)
kinetic   (Body _ (Vel x y z)) = (abs x) + (abs y) + (abs z)

energy :: Body -> Int
energy x = potential x * kinetic x

instance Semigroup Int where
    (<>) = (+)

instance Monoid Int where
    mempty = 0
    mappend = (+)

body :: Pos -> Body
body p = Body p (Vel 0 0 0)

main :: IO ()
main = let io = Pos (-5) 6 (-11)
           europa = Pos (-8) (-4) (-2)
           ganymede = Pos 1 16 4
           callysto = Pos 11 11 (-4)
           bodies = map body [io, europa, ganymede, callysto]
           movement = zip [0..] (iterate step bodies)
        in do putStrLn "=== Task 1 ==="
              let target = snd . head . drop 1000 $ movement
              print $ foldMap energy target


