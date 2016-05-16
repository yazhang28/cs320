---------------------------------------------------------------------
--
-- CAS CS 320, Fall 2015
-- Assignment 5 (skeleton code)
-- Graph.hs
--
{-# LANGUAGE NPlusKPatterns #-}
module Graph where

data Graph a =
   Choices a (Graph a, Graph a)
   | Outcome a
   deriving (Eq, Show)
   
class State a where
   outcome :: a -> Bool
   choices :: a -> (a, a)

--  ... Complete for Problem #3, part (a) ...
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a,a1) = (f(a), f(a1))

--  ... Complete for Problem #3, part (b) ...
state :: (Graph a) -> a
state (Choices a (a2, a3)) = a
state (Outcome a) =  a

-- If states can be compared, then graphs containing
-- those states can be compared by comparing the
-- states in the respective root nodes.
instance Ord a => Ord (Graph a) where
  g `compare` g' = state g `compare` state g'  -- Complete for Problem #3, part (c).
  
-- Complete Problem #3, parts (d-g).
graph :: State a => a -> Graph a
graph a = if outcome a == True then Outcome a else Choices a (mapTuple graph (choices a))

depths :: Integer -> Graph a -> [Graph a]
depths (0) (a) = [a]
depths (1) (Outcome a) = [Outcome a]
depths (n) (Outcome a) = [Outcome a]
depths (1) (Choices a (b,b2)) = [b,b2]
depths (n) (Choices a (b,b2)) = depths (n-1) (b) ++ depths (n-1) (b2)

fold :: (a -> b) -> (a -> (b,b) -> b) -> Graph a -> b
fold o c (Outcome a) = o a
fold o c (Choices a (b,b2)) = c a((fold o c b),(fold o c b2))

nodes = fold (\o -> 1) (\s (i,i') -> 1 + i + i') 
--numofc (Outcome a) = 0
--numofc (Choices a (b,b2)) = 1 + numofc(b) + numofc(b2)

m (g) = depth (nodes g)
depth x = floor (logBase 2 x)

outcomes :: State a => Graph a -> [Graph a]
outcomes a = depths (m a) a
--eof