fuel :: Int -> Int
fuel = (max 0) . (\x -> x - 2) . (`div` 3)

fullFuel :: Int -> Int
fullFuel = sum . takeWhile (> 0) . tail . iterate fuel

main :: IO ()
main = do inp <- getContents
          let result = sum . map fullFuel . map read . lines $ inp
          print result
