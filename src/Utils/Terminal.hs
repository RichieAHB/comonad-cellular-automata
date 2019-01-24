module Utils.Terminal where

import System.Console.Terminal.Size

-- get terminal size (will crash if not there)
termSize :: IO (Int, Int)
termSize = do
  Just (Window h w) <- size
  return (fromIntegral h, fromIntegral w)
