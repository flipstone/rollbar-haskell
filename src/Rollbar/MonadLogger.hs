{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Rollbar.MonadLogger (reportErrorS) where

import BasicPrelude

import Rollbar hiding (reportErrorS)

import GHC.Stack (CallStack)

default (Text)

-- | report errors to rollbar.com and log them with monad-logger
reportErrorS ::
    Settings ->
    Options ->
    -- | log section
    Text ->
    -- | monad-logger logging function. takes a section and a message
    (Text -> Text -> IO ()) ->
    Maybe CallStack ->
    -- | message
    Text ->
    IO ()
reportErrorS = reportLoggerErrorS
