-- |
-- Module      : Grepl.Watcher
-- Copyright   : (c) 2026 Tony Day
-- License     : BSD-3-Clause
-- Maintainer  : tonyday567@gmail.com
--
-- File watching utilities for monitoring markdown output logs from GHCi sessions.
--
-- = Overview
--
-- @Grepl.Watcher@ watches a directory for markdown (.md) file changes,
-- enabling agents to react to GHCi query results as they appear in logs.
--
-- Uses FSNotify to detect file events and streams filepaths through a
-- 'TChan', allowing agents to coordinate queries and responses in real-time.
--
-- = Usage
--
-- Watch a directory and react to changes:
--
-- > import Grepl.Watcher
-- > chan <- watchMarkdown "./log"
-- > -- Now read file paths from chan as markdown files are modified
--
-- Or provide your own 'TChan':
--
-- > import Control.Concurrent.STM (newTChanIO)
-- > chan <- newTChanIO
-- > watchMarkdownWith "./log" chan
-- > -- chan now receives file paths as they change
module Grepl.Watcher
  ( watchMarkdown,
    watchMarkdownWith,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, writeTChan)
import Control.Monad (forever)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import System.FSNotify
import System.FilePath (takeExtension, takeFileName)

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
    _ <-
      watchDir
        mgr
        watchdir
        (isJust . isMarkdownEvent) -- predicate: only fire on matching events
        (handleEvent chan) -- action: push to chan
        -- Keep watcher thread alive
    forever $ threadDelay 1000000
  pure ()

-- | Handle a file event by pushing the filepath to the TChan
handleEvent :: TChan String -> Event -> IO ()
handleEvent chan e =
  case isMarkdownEvent e of
    Just fp -> atomically $ writeTChan chan fp
    Nothing -> pure ()
