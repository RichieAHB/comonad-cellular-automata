module Utils.Cell where

-- convert a cell to a char
toChar :: Bool -> Char
toChar b = if b then '#' else ' '
