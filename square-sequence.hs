-- https://www.codewars.com/kata/5886d65e427c27afeb0000c1/solutions/haskell
module SquareSec where
    squareDigitsSequence :: Int -> Int
    squareDigitsSequence x = (length (getUniqueSquareSequence x)) + 2
    
    stopOnDupe z = foldr (\x acc -> x : takeWhile (/= x) acc) [] z
    
    getUniqueSquareSequence a = stopOnDupe (takeWhile (/= a) (getSquareSequence a))

    getSquareSequence a = let cur = squaredTotal a
                            in cur : getSquareSequence cur

    squaredTotal a = let sq x = x*x 
                        in sum (map sq (getNums (fromIntegral a) 0))

    getNums x i
        | 10^i > x = [] 
        | otherwise = mod divided 10 : getNums x (i + 1)
                where divided = (floor (x / (10^i))) :: Int
