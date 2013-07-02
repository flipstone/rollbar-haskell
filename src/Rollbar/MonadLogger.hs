{-# Language NoImplicitPrelude, FlexibleContexts, OverloadedStrings, ExtendedDefaultRules #-}
module Rollbar.MonadLogger where

import BasicPrelude
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Network.BSD (HostName)

import Rollbar

default (Text)


-- | report errors to rollbar.com and log them with monad-logger
reportErrorS :: (MonadIO m, MonadBaseControl IO m)
             => Text -- ^ environment (development, production, etc)
             -> HostName -- ^ computer
             -> Text -- ^ log section
             -> (Text -> LoggingT m ()) -- ^ monad-logger logging function
             -> Text -- ^ message
             -> LoggingT m ()
reportErrorS = reportLoggerErrorS
    