module Rollbar.MonadLogger
  ( reportErrorS
  ) where

import Data.Text qualified as T
import GHC.Exception (CallStack)
import Rollbar qualified

-- | report errors to rollbar.com and log them with monad-logger
reportErrorS ::
  Rollbar.Settings ->
  Rollbar.Options ->
  -- | log section
  T.Text ->
  -- | monad-logger logging function. takes a section and a message
  (T.Text -> T.Text -> IO ()) ->
  Maybe CallStack ->
  Rollbar.ErrorLevel ->
  -- | message
  T.Text ->
  IO ()
reportErrorS = Rollbar.reportLoggerErrorS
