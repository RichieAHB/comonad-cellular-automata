module SierpinskiTriangles
    ( run
    ) where

import Utils.Cell
import Data.CellularAutomata.CA1
import Control.Comonad

-- the cellular automata rule (for Bools)
-- not exhaustive on finite lists
rule :: CA1 Bool -> Bool
rule (CA1 (l:_) c (r:_)) = not (l && c && not r || (l == c))

start :: CA1 Bool
start = center True False

run :: IO ()
run =
  putStrLn
    $   unlines
    $   take 50
    $   stringify toChar        50
    <$> iterate   (extend rule) start
