import Data.List (sort)

solve :: String -> Int
solve = uncurry totalDistance . parse

parse :: String -> ([Int], [Int])
parse = unzip . map (toPair . map read . words) . lines
  where
    toPair [x, y] = (x, y)

totalDistance xs ys =
  sum $ zipWith distance (sort xs) (sort ys)

distance :: Int -> Int -> Int
distance x y = abs (x - y)

main :: IO ()
main = do
  input <- readFile "opdracht1-input.txt"
  print $ solve input
