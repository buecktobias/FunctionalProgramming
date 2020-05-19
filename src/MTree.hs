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

data Graph = Graph {node::[Node]}

data Edge = Edge{n1:: Node, n2::Node, cost::Int}

data Node = Node{edges::[Edge], nValue::Int}

