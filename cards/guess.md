{-# LANGUAGE OverloadedStrings #-}

module MinimalStatefulRepl
  ( ReplSession
  , startRepl
  , sendCommand
  , closeRepl
  , ReplResult(..)
  ) where

import System.Posix.Pty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Exit (ExitCode(..))
import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Aeson (ToJSON(..), object, (.=), encode)
import qualified Data.ByteString.Lazy as BSL

-- | Structured output for the agent (JSON-friendly)
data ReplResult = ReplResult
  { command     :: Text
  , output      :: Text
  , prompt      :: Text
  , success     :: Bool
  , durationMs  :: Int
  , errorMsg    :: Maybe Text
  } deriving (Show, Eq)

instance ToJSON ReplResult where
  toJSON r = object
    [ "command"    .= command r
    , "output"     .= output r
    , "prompt"     .= prompt r
    , "success"    .= success r
    , "durationMs" .= durationMs r
    , "errorMsg"   .= errorMsg r
    ]

-- | A live REPL session the agent can interact with turn-by-turn
data ReplSession = ReplSession
  { pty        :: Pty
  , currentPrompt :: Text
  , timeoutMs  :: Int
  }

-- | Start a new stateful REPL session (e.g. "cabal repl mylib")
startRepl :: FilePath -> [String] -> Text -> Int -> IO ReplSession
startRepl cmd args initialPrompt timeoutMs = do
  (pty, _, _) <- spawnWithPty Nothing 80 24 cmd args
  -- Wait for initial prompt to appear
  threadDelay 200000  -- 200ms
  pure $ ReplSession pty initialPrompt timeoutMs

-- | Send one command and wait for the REPL to finish processing it
sendCommand :: ReplSession -> Text -> IO ReplResult
sendCommand sess cmd = do
  start <- getCurrentTime
  writePty (pty sess) (TE.encodeUtf8 cmd <> "\n")

  -- Smart waiting: read until we see the prompt again (or timeout)
  output <- readUntilPrompt (pty sess) (currentPrompt sess) (timeoutMs sess)
  end <- getCurrentTime
  let dur = round $ diffUTCTime end start * 1000

  case output of
    Left err -> pure $ ReplResult cmd "" (currentPrompt sess) False dur (Just err)
    Right (out, newPrompt) -> do
      let cleaned = cleanOutput out (currentPrompt sess)
      pure $ ReplResult cmd cleaned newPrompt True dur Nothing

-- | Read from PTY until we see the expected prompt (or timeout)
readUntilPrompt :: Pty -> Text -> Int -> IO (Either Text (Text, Text))
readUntilPrompt pty expectedPrompt timeoutMs = do
  let loop acc startTime = do
        now <- getCurrentTime
        if diffUTCTime now startTime > fromIntegral timeoutMs / 1000
          then pure $ Left "Timeout waiting for prompt"
          else do
            chunk <- BS.hGetSome (ptyRead pty) 4096
            if BS.null chunk
              then do
                threadDelay 50000  -- 50ms
                loop acc startTime
              else do
                let acc' = acc <> chunk
                    txt = TE.decodeUtf8 acc'
                -- Check if any known prompt appears
                case detectPrompt txt of
                  Just p  -> pure $ Right (txt, p)
                  Nothing -> loop acc' startTime
  start <- getCurrentTime
  loop BS.empty start

-- | Detect any known GHCi prompt (extend this list as needed)
detectPrompt :: Text -> Maybe Text
detectPrompt txt =
  let prompts = ["ghci> ", "Prelude> ", "λ> ", "> ", "Ok, modules loaded: "]
  in foldr (\p acc -> if p `T.isInfixOf` txt then Just p else acc) Nothing prompts

-- | Remove the prompt and any echoed command from the output
cleanOutput :: Text -> Text -> Text
cleanOutput txt prompt =
  let noEcho = T.replace (prompt <> "\n") "" txt
      noPrompt = T.replace prompt "" noEcho
  in T.strip noPrompt

-- | Gracefully close the session
closeRepl :: ReplSession -> IO ()
closeRepl sess = do
  writePty (pty sess) ":quit\n"
  threadDelay 100000
  -- pty is automatically closed when the process exits
