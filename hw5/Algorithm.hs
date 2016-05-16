---------------------------------------------------------------------
--
-- CAS CS 320, Fall 2015
-- Assignment 5 (skeleton code)
-- Algorithm.hs
--

module Algorithm where

import Graph

type Algorithm a = Graph a -> Graph a

-- Complete Problem #4, parts (a-f).
greedy :: Ord a => Algorithm a
greedy a = minimum(depths 1 (a))

patient :: Ord a => Integer -> Algorithm a 
patient 0 (a) = a
patient n (a) = minimum (depths n (a))

optimal :: (State a, Ord a) => Algorithm a
optimal a = minimum (outcomes a)

metaCompose :: Algorithm a -> Algorithm a -> Algorithm a
metaCompose a1 a2 a = a1 (a2 a)

metaRepeat :: Integer -> Algorithm a -> Algorithm a
metaRepeat 0 a1 a = a
metaRepeat 1 a1 a = a1 (a)
metaRepeat n a1 a = a1 (metaRepeat (n-1) a1 a)

metaGreedy :: Ord a => Algorithm a -> Algorithm a -> Algorithm a
metaGreedy a1 a2 a = if a1 a <= a2 a then a1 a else a2 a

-- Problem #4, part (g).
--less calls made to patient with impatient.  longer runtime using impatient

---------------------------------------------------------------------
-- Problem #6 (extra extra credit).

-- An embedded language for algorithms.
data Alg =
    Greedy
  | Patient Integer
  | Impatient Integer
  | Optimal
  | MetaCompose Alg Alg
  | MetaRepeat Integer Alg
  | MetaGreedy Alg Alg
  deriving (Eq, Show)

interpret :: (State a, Ord a) => Alg -> Algorithm a
interpret _ = \g -> g -- Replace for Problem #6, part (a).

data Time =
    N Integer 
  | Infinite
  deriving (Eq, Show)

-- instance Num Time where
--   ... Complete for Problem #6, part (b).

performance :: Alg -> Time
performance _ = N 0 -- Replace for Problem #6, part (c).

--eof