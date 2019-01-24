module Data.CellularAutomata.CA2
    ( CA2(..),
      stringify,
      set
    ) where

import Control.Comonad
import Data.CellularAutomata.CA1 (CA1(..), left, right)
import qualified Data.CellularAutomata.CA1 as CA1

newtype CA2 a = CA2 (CA1 (CA1 a)) deriving (Show)

instance Functor CA2 where
    fmap f (CA2 ca) = CA2 $ fmap (fmap f) ca

-- adds another layer to the comonad that "replicates" two layers down
nudge :: CA1 (CA1 a) -> CA1 (CA1 (CA1 a))
nudge ca =
  CA1 (tail (iterate (fmap left) ca)) ca (tail (iterate (fmap right) ca))

instance Comonad CA2 where
    extract (CA2 ca) = extract (extract ca)
    duplicate (CA2 ca) = fmap CA2 $ CA2 $ nudge $ nudge ca

-- convert a CA2 into a list
toList :: Int -> Int -> CA2 a -> [[a]]
toList h w (CA2 ca) = fmap (CA1.toList w) (CA1.toList h ca)

-- stringify a 2D
stringify :: (a -> Char) -> Int -> Int -> CA2 a -> String
stringify f h w = unlines . fmap (fmap f) . toList h w

-- helper for building
set :: CA1 a -> [CA1 a] -> CA1 a -> [CA1 a] -> CA1 a -> CA2 a
set ti ts c bs bi = CA2 (CA1.set ti ts c bs bi)
