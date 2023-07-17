{-# Language NoImplicitPrelude, FlexibleContexts, OverloadedStrings, ExtendedDefaultRules #-}
module Rollbar.MonadLogger where

import BasicPrelude

import Rollbar

import GHC.Stack (CallStack)

default (Text)


-- | report errors to rollbar.com and log them with monad-logger
reportErrorS :: Settings
             -> Options
             -> Text -- ^ log section
             -> (Text -> Text -> IO ()) -- ^ monad-logger logging function. takes a section and a message
             -> Maybe CallStack
             -> Text -- ^ message
             -> IO ()
reportErrorS = reportLoggerErrorS

