module GameOfLife
    ( run
    ) where

import Utils.Cell
import Utils.Terminal
import Control.Monad
import Control.Monad.Utils
import Control.Comonad
import Data.CellularAutomata.CA2
import Data.CellularAutomata.CA1 hiding (stringify, set)
import qualified Data.CellularAutomata.CA1 as CA1


-- the cellular automata rule (for Bools)
rule :: CA2 Bool -> Bool
rule (CA2 (CA1 (CA1 (c0:_) c1 (c2:_):_) (CA1 (c3:_) c4 (c5:_)) (CA1 (c6:_) c7 (c8:_):_)))
  = let n = length $ filter id [c0, c1, c2, c3, c5, c6, c7, c8]
    in  c4 && (n == 2 || n == 3) || not c4 && n == 3

start :: CA2 Bool
start = set (CA1.fill False)
            [CA1.center True False]
            (CA1.set False [] False [True] False)
            [CA1.set False [True] True [True] False]
            (CA1.fill False)

run :: IO ()
run = do
  (h, w) <- termSize
  runForever 100
    $   putStrLn
    <$> stringify toChar (h `quot` 2) (w `quot` 2)
    <$> iterate (extend rule) start
