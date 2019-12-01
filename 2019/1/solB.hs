main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve xs = foldr (+) 0 (map solveb xs)

solveb :: Int -> Int
solveb n = let 
                m = n `div` 3 - 2
           in 
              if m > 0
              then  m + solveb m
              else 0
