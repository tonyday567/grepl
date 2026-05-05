module Grepl.Repl where

import Control.Arrow (Kleisli (..), runKleisli)
import Control.Category ((>>>))
import Control.Monad (when)
import Circuit

-- $setup
-- >>> import Control.Arrow (Kleisli (..))
-- >>> import Circuit (Circuit (Lift), reify)
-- >>> let countDown n = Lift (Kleisli (\_ -> pure (n - 1)))

-- | read -> evaluate -> write -> loop

type Wire = Circuit (Kleisli IO) Either

type Commit input = Wire input ()
type Emit output = Wire () output

type Wire' = Circuit (Kleisli IO) (,)

readLine :: Emit String
readLine = Lift (Kleisli (\_ -> readLn))

printLine :: Commit String
printLine = Lift (Kleisli (\s -> putStrLn s >> return ()))

liftEval :: (String -> String) -> Wire String String
liftEval eval = Lift (Kleisli (\s -> return (eval s)))

-- | Convenient wrapper for simple IO feedback loops.
--
--   @loopIO step@ creates a Circuit that runs @step@ for each iteration,
--   treating both entry and loop states uniformly.
loopIO :: (a -> IO (Either a b)) -> Circuit (Kleisli IO) Either a b
loopIO step = Loop (Kleisli (\case
  Right x -> step x
  Left  x -> step x))

-- | Loop an Emit wire until a predicate is satisfied.
--
--   Lowers the Emit circuit, runs it repeatedly in a loop, and lifts the
--   decision back into the feedback structure via Loop.
until :: (a -> Bool) -> Emit a -> Circuit (Kleisli IO) Either () a
until p wire = Loop (Kleisli (\case
  Right () -> runKleisli (reify wire) () >>= \a -> return (if p a then Right a else Left ())
  Left  () -> runKleisli (reify wire) () >>= \a -> return (if p a then Right a else Left ())))

-- | Compose a wire with a predicate test and optional logging.
--
--   Runs: wire -> test predicate -> log if true -> return predicate result
replLoop :: (Show a) => (a -> Bool) -> Emit a -> Wire () Bool
replLoop pred wire = 
  Lift (Kleisli (\() -> runKleisli (reify wire) () >>= \a -> 
    return (pred a, a))) >>>
  Lift (Kleisli (\(b, a) -> 
    when b (putStrLn $ "result: " ++ show a) >> 
    return b))

-- | Loop replLoop until predicate returns False.
--
-- >>> import Data.IORef
-- >>> counter <- newIORef (0 :: Int)
-- >>> let countingEmit = Lift (Kleisli (\_ -> do { c <- readIORef counter; modifyIORef counter (+1); pure c }))
-- >>> result <- runKleisli (reify (loopUntilFalse (== 2) countingEmit)) ()
-- result: 2
-- >>> result
-- ()
loopUntilFalse :: (Show a) => (a -> Bool) -> Emit a -> Wire () ()
loopUntilFalse pred wire = Loop $
  Kleisli (\fb -> 
    runKleisli (reify wire) () >>= \a -> 
    let b = pred a
    in when b (putStrLn $ "result: " ++ show a) >> 
       return (if b then Right () else Left ()))

