#!/usr/bin/env runhaskell

import Repl
import Control.Concurrent (threadDelay)
import System.Process (waitForProcess)

main :: IO ()
main = do
  putStrLn "Booting channel defaultChannelConfig..."
  h <- channel defaultChannelConfig
  putStrLn "✓ Channel started, process handle acquired"
  
  putStrLn "Waiting 5 seconds..."
  threadDelay 5000000
  
  putStrLn "Checking logs..."
  
  putStrLn "\n=== stdout ==="
  stdout <- readFile "./log/cabal-repl-stdout.md"
  putStrLn $ "Lines: " ++ show (length (lines stdout))
  putStrLn $ "First 500 chars: " ++ take 500 stdout
  
  putStrLn "\n=== stderr ==="
  stderr <- readFile "./log/cabal-repl-stderr.md"
  putStrLn $ "Lines: " ++ show (length (lines stderr))
  putStrLn $ "First 500 chars: " ++ take 500 stderr
  
  putStrLn "\nTest complete."
