module Integral
(solve)
 where

power:: (Num a) => a -> Int -> a
power x 1 = x
power x n = x * power x (n-1)

add:: Int -> Double -> Double
add a b = fromIntegral a + b

multiply:: (Num a) => a -> a -> a
multiply a b = a * b


mFuncE :: Double -> Int -> Int -> Double
mFuncE x a b = multiply (fromIntegral a)  (power x b)


mFunc :: Double -> [Int] -> [Int] -> Double
mFunc x [] [] = 0
mFunc x (a:as) (b:bs) = mFuncE x a b + mFunc x as bs

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> Double
solve l r a b = result
                where
                xs = [ (fromIntegral l + ((fromIntegral i) / 1000)) | i <- [1 .. (r - l) * 1000]]
                funcResults = map (\x -> (mFunc x a b) / 1000) xs
                result = sum funcResults
