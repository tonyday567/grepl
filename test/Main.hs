module Main where

import Repl
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  putStrLn "Starting repl in repl project..."
  h <- startRepl "."

  putStrLn "Waiting for GHCi to boot..."
  threadDelay 8000000  -- 8 seconds

  putStrLn "\nQuery 1: :t fmap"
  result1 <- queryRepl ":t fmap" "fmap ::"
  case result1 of
    Just t -> putStrLn $ "  ✓ " ++ t
    Nothing -> putStrLn "  ✗ timeout"

  putStrLn "\nQuery 2: :k Maybe"
  result2 <- queryRepl ":k Maybe" "* -> *"
  case result2 of
    Just k -> putStrLn $ "  ✓ " ++ k
    Nothing -> putStrLn "  ✗ timeout"

  putStrLn "\nQuery 3: :t run"
  result3 <- queryRepl ":t run" "run ::"
  case result3 of
    Just r -> putStrLn $ "  ✓ " ++ r
    Nothing -> putStrLn "  ✗ timeout"

  putStrLn "\nDone."
