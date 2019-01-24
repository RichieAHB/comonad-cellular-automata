module Control.Monad.Utils where

import Control.Concurrent

-- runs an infinite list of IO forever with a delay between each iteration
runForever :: Int -> [IO ()] -> IO ()
runForever d (m:ms) = do
  m
  threadDelay $ d * 1000
  runForever d ms
