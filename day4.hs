import Control.Monad
import Data.List

meets :: String -> Bool
meets s = all (uncurry (<=)) (pairs s) && any (== 2) (lrc s)

pairs = zip <*> tail

lrc :: Eq a => [a] -> [Int]
lrc []       = [0]
lrc (x : xs) = lrc' 1 x xs
            where lrc' acc x' (x'' : rest) | x' == x'' = lrc' (acc + 1) x'' rest
                                           | otherwise = acc : (lrc' 1 x'' rest)
                  lrc' acc _ []                        = [acc]

-- Count the number of times a predicate is true

count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where go n [] = n
        go n (x:xs) | p x       = go (n+1) xs
                    | otherwise = go n xs

main :: IO ()
main = print $ count meets . map show $ [240920..789857]
