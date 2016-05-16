---------------------------------------------------------------------
--
-- CAS CS 320, Fall 2015
-- Assignment 5 (skeleton code)
-- SuperString.hs
--

module SuperString where

import Data.List (isPrefixOf)
import Graph
import Algorithm

data SuperString =
    SuperStr String [String]
  deriving (Eq, Show)

-- To merge two strings, take the longest suffix of the first string
-- that overlaps with the second, and replace it with the second string.
merge :: String -> String -> String
merge (x:xs) ys  = if isPrefixOf (x:xs) ys then ys else x:(merge xs ys)
merge []     ys  = ys

meas (SuperStr s []) = length s
meas (SuperStr s (x:xs)) = length s

--   ... Complete for Problem #2, part (a) ...
instance Ord SuperString where
   s `compare` s' =  meas s `compare` meas s'

str (s,(x:xs)) = merge s ((x:xs)!!0)
str1 ((x:xs),s) = merge ((x:xs)!!0) s

--   ... Complete for Problem #2, part (b) ...  
instance State SuperString where
   outcome (SuperStr s []) = True
   outcome (SuperStr s _) = False
   choices (SuperStr s (x:xs)) = (SuperStr (str (s,(x:xs))) (drop 1(x:xs)), SuperStr (str1 ((x:xs),s)) (drop 1(x:xs)))


--eof