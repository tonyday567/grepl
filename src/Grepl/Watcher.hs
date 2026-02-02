module Grepl.Watcher
  ( watchMarkdown
  , watchMarkdownWith
  ) where

import System.FSNotify
import System.FilePath (takeExtension, takeFileName)
import Control.Concurrent.Async (async)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TChan, newTChanIO, writeTChan, atomically)
import Data.Maybe (isJust)
import Data.List (isInfixOf)

-- | Check if filename is markdown (.md extension)
isMarkdownFile :: FilePath -> Bool
isMarkdownFile fp = takeExtension fp == ".md"

-- | Check if filename is stdout (not stderr)
isStdout :: FilePath -> Bool
isStdout fp = not ("stderr" `isInfixOf` takeFileName fp)

-- | Filter for .md file events (Added or Modified only, stdout files only)
isMarkdownEvent :: Event -> Maybe FilePath
isMarkdownEvent (Added fp _ IsFile) = 
  if isMarkdownFile fp && isStdout fp then Just fp else Nothing
isMarkdownEvent (Modified fp _ IsFile) = 
  if isMarkdownFile fp && isStdout fp then Just fp else Nothing
isMarkdownEvent _ = Nothing

-- | Watch a directory for .md file changes, push filepaths to a TChan
--
-- Returns the TChan for reading file paths as they're detected.
-- Runs the watcher in a background async thread.
watchMarkdown :: FilePath -> IO (TChan String)
watchMarkdown watchdir = do
  chan <- newTChanIO
  watchMarkdownWith watchdir chan
  pure chan

-- | Watch a directory for .md file changes, push to provided TChan
--
-- Launches the watcher in a background async thread.
-- Returns immediately.
watchMarkdownWith :: FilePath -> TChan String -> IO ()
watchMarkdownWith watchdir chan = do
  _ <- async $ withManager $ \mgr -> do
    -- Set up the watcher with filter and handler
    _ <- watchDir 
      mgr 
      watchdir 
      (isJust . isMarkdownEvent)  -- predicate: only fire on matching events
      (handleEvent chan)           -- action: push to chan
    -- Keep watcher thread alive
    forever $ threadDelay 1000000
  pure ()

-- | Handle a file event by pushing the filepath to the TChan
handleEvent :: TChan String -> Event -> IO ()
handleEvent chan e = 
  case isMarkdownEvent e of
    Just fp -> atomically $ writeTChan chan fp
    Nothing -> pure ()
