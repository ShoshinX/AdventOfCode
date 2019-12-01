main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve xs = foldr (+) 0 (map solveb xs)

solveb :: Int -> Int
solveb n = n `div` 3 - 2

