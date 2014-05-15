{-# Language NoImplicitPrelude, FlexibleContexts, OverloadedStrings, ExtendedDefaultRules #-}
module Rollbar.MonadLogger where

import BasicPrelude
import Control.Monad.Trans.Control (MonadBaseControl)

import Rollbar

default (Text)


-- | report errors to rollbar.com and log them with monad-logger
reportErrorS :: (MonadIO m, MonadBaseControl IO m)
             => Settings
             -> Options
             -> Text -- ^ log section
             -> (Text -> Text -> m ()) -- ^ monad-logger logging function. takes a section and a message
             -> Text -- ^ message
             -> m ()
reportErrorS = reportLoggerErrorS
    
