module Rollbar.MonadLogger
  ( reportErrorS
  ) where

import qualified Data.Text as T
import qualified GHC.Stack as Stack

import qualified Rollbar

-- | report errors to rollbar.com and log them with monad-logger
reportErrorS ::
  Rollbar.Settings ->
  Rollbar.Options ->
  -- | log section
  T.Text ->
  -- | monad-logger logging function. takes a section and a message
  (T.Text -> T.Text -> IO ()) ->
  Maybe Stack.CallStack ->
  -- | message
  T.Text ->
  IO ()
reportErrorS = Rollbar.reportLoggerErrorS
