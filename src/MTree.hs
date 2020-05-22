module MTree where

import Data.Maybe (isNothing)

data Tree = Tree {value::Int, leftTree::Maybe Tree, rightTree::Maybe Tree} deriving (Show, Ord, Eq)

creteTree:: Int -> Tree
creteTree v = Tree v Nothing Nothing

setLeftTree:: Tree -> Tree -> Tree
setLeftTree t newLeftTree = t {leftTree = Just newLeftTree}

setRightTree:: Tree -> Tree -> Tree
setRightTree t newRightTree = t {rightTree = Just newRightTree}

unMaybe:: Maybe Tree -> Tree
unMaybe (Just m) = m


addElement:: Tree -> Int -> Tree
addElement t n
               | n < (value t) = if isNothing (leftTree t) then setLeftTree t (creteTree n) else addElement (unMaybe (leftTree t)) n
               | otherwise= if isNothing (rightTree t) then setRightTree t (creteTree n) else addElement (unMaybe (rightTree t)) n

data Graph = Graph {nodes::[Node]} deriving Show

getNode:: Int -> Graph -> Node
getNode n1 g = head (filter (\n -> nValue n == n1) (nodes g))

addNode:: Node -> Graph -> Graph
addNode n g= g {nodes = (nodes g) ++ [n]}

data Node = Node{neighbours::[Node], nValue::Int} deriving Show

createNode:: Int -> Node
createNode n = Node [] n

bfs:: Node -> Int -> [Node]
bfs n dest 
  | (nValue n) == dest = [n]
  | otherwise= 
              let
              bfsss = (map (\x -> bfs x dest)( neighbours n))
              bfsssLengths = map length bfsss
              shortestLength = if length bfsssLengths == 0 then 0 else  minimum bfsssLengths
              shortestPath = if length bfsssLengths == 0 then [] else head (filter (\x -> length x == shortestLength) bfsss)
              in
              [n] ++ shortestPath

addEdge:: Node -> Node -> (Node, Node)
addEdge n1 n2 = (n1 {neighbours = ((neighbours n1) ++ [n2])}, n2 {neighbours = ((neighbours n2) ++ [n1])})


