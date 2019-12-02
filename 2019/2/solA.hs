import Data.List.Split
main = interact $ show . solve' . splitOn ","

solve' :: [String] -> [String]
solve' xs = solve xs xs

solve :: [String] -> [String] -> [String]
solve ("99":xs) bs = bs
solve (x1:x2:x3:x4:xs) bs = 
    case x1 of
        "1"     -> solve xs (lhs ++ [(add arg1 arg2)] ++ rhs)
        "2"     -> solve xs (lhs ++ [(mul arg1 arg2)] ++ rhs)
        _       -> error "invalid opcode"
    where
        lhs     = (take (read x4) bs)
        rhs     = (drop ((read x4)+1) bs)
        arg1    = bs !! (read x2)
        arg2    = bs !! (read x3)

add :: String -> String -> String
add a1 a2 = show $ (read a1) + (read a2)

mul :: String -> String -> String
mul a1 a2 = show $ (read a1) * (read a2)
