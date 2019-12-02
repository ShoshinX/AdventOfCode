import Data.List.Split
main = interact $ show . solve' . splitOn ","

solve' :: [String] -> [String]
solve' xs = 
    let 
        bs = solve xs xs
    in
        if (read (bs !! 0)) > 19690720
        then solve''    (minus1to1 xs)
        else solve'     (add1to1 xs)

solve'' xs = 
    let
        bs = solve xs xs
    in
        if (read (bs !! 0)) == 19690720
        then xs
        else solve'' (add1to2 xs)

add1to1 :: [String] -> [String]
add1to1 xs = (take 1 xs) ++ [show ((read (xs !! 1)) + 1)] ++ (drop 2 xs)

minus1to1 :: [String] -> [String]
minus1to1 xs = (take 1 xs) ++ [show ((read (xs !! 1)) - 1)] ++ (drop 2 xs)

set0to1 :: [String] -> [String]
set0to1 xs = ["1"] ++ (drop 1 xs)

add1to2 :: [String] -> [String]
add1to2 xs = (take 2 xs) ++ [show ((read (xs !! 2)) + 1)] ++ (drop 3 xs)

solve :: [String] -> [String] -> [String]
solve ("99":xs) bs = bs
solve (x1:x2:x3:x4:xs) bs = 
    case x1 of
        "1"     -> solve xs (lhs ++ [(add arg1 arg2)] ++ rhs)
        "2"     -> solve xs (lhs ++ [(mul arg1 arg2)] ++ rhs)
        _       -> error (x1 ++ " " ++ x2 ++ " " ++ x3 ++ " " ++ x4)
    where
        lhs     = (take (read x4) bs)
        rhs     = (drop ((read x4)+1) bs)
        arg1    = bs !! (read x2)
        arg2    = bs !! (read x3)

add :: String -> String -> String
add a1 a2 = show $ (read a1) + (read a2)

mul :: String -> String -> String
mul a1 a2 = show $ (read a1) * (read a2)
