module Data.CellularAutomata.CA1
    ( CA1(..),
      fill,
      set,
      center,
      stringify,
      toList,
      left,
      right
    ) where

import           Control.Comonad

-- The constructor operators for 1D cellular automata

data CA1 a = CA1 [a] a [a] deriving (Show)

instance Functor CA1 where
    fmap f (CA1 ls c rs) = CA1 (fmap f ls) (f c) (fmap f rs)

instance Comonad CA1 where
    extract (CA1 _ x _) = x
    duplicate ca = CA1 (tail (iterate left ca)) ca (tail (iterate right ca))

-- move the comonad right
right :: CA1 a -> CA1 a
right (CA1 ls c (r:rs)) = CA1 (c : ls) r rs

-- move the comonad left
left :: CA1 a -> CA1 a
left (CA1 (l:ls) c rs) = CA1 ls l (c : rs)

-- convert a comonad to a list
toList :: Int -> CA1 a -> [a]
toList m (CA1 ls c rs) = reverse (take m ls) ++ [c] ++ take m rs

-- convert the list to a string with a func
stringify :: (a -> Char) -> Int -> CA1 a -> String
stringify f n = fmap f . toList n

-- helpers to construct CA1s
set :: a -> [a] -> a -> [a] -> a -> CA1 a
set li ls c rs ri = CA1 (reverse ls ++ repeat li) c (rs ++ repeat ri)

center :: a -> a -> CA1 a
center c o = set o [o] c [o] o

fill :: a -> CA1 a
fill v = center v v
