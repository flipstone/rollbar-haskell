module Rollbar.MonadLogger where

import Control.Monad.Logger

reportErrorS :: (MonadLogger logger, MonadIO m, MonadBaseControl IO m)
             => (Text -> logger (m ())) -> Text -> logger (m ())
reportErrorS hostName env logger msg = do
    logger msg
    