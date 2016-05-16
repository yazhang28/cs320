---------------------------------------------------------------------
--
-- CAS CS 320, Fall 2015
-- Assignment 5 (skeleton code)
-- BinPacking.hs
--

module BinPacking where

import Graph
import Algorithm

type Item = Integer
type Bin = Integer

data BinPacking =
  BinPack Bin Bin [Item]
  deriving (Eq, Show)

dif :: BinPacking -> Integer
dif (BinPack b b2 []) = abs(b - b2)
dif (BinPack b b2 (x:xs)) = abs(b - b2)

-- ... Complete for Problem #1, part (a) ...
instance Ord BinPacking where
  b `compare` b' = dif b `compare` dif b'

-- ... Complete for Problem #1, part (b) ... 
instance State BinPacking where
  outcome (BinPack b b2 []) = True
  outcome (BinPack b b2 (x:xs)) = False
  choices (BinPack b b2 (x:xs)) = (BinPack (b+(x:xs)!!0) b2 (drop 1(x:xs)) , BinPack b (b2+(x:xs)!!0) (drop 1(x:xs))) 


--eof