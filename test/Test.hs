{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow (Kleisli (..), runKleisli)
import Data.IORef
import Circuit
import Grepl.Repl

main :: IO ()
main = do
  putStrLn "Test: loopUntilFalse that loops and fires"
  counter <- newIORef (0 :: Int)
  let countingEmit = Lift (Kleisli (\_ -> do
        c <- readIORef counter
        modifyIORef counter (+1)
        putStrLn $ "  [emit] iteration " ++ show c
        pure c))
  
  putStrLn "Running loopUntilFalse, exiting when count == 2"
  result <- runKleisli (reify (loopUntilFalse (== 2) countingEmit)) ()
  putStrLn $ "Result: " ++ show result
  putStrLn "Done"
