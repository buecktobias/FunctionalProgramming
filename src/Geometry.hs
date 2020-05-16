module Geometry where

data Vector = Vector {x::Int, y::Int}

distanceBetween:: Vector -> Vector -> Double
distanceBetween (Vector x1 y1) (Vector x2 y2) = sqrt (fromIntegral (((x1 - x2) ^ 2) + ((y1 -y2) ^ 2)))

infix 5 <==>
(<==>) :: Vector -> Vector -> Double
v1 <==> v2 = distanceBetween v1 v2

data Polygon = Polygon {points::[Vector]}

perimeter:: Polygon -> Double
perimeter p =  sum (zipWith distanceBetween (points p) (drop 1 (take ((length (points p)) + 1) (cycle (points p)))))

area :: Polygon -> Double
area p = 3.0