{-# Language NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, FlexibleContexts, ScopedTypeVariables #-}
-- | Main entry point to the application.
module Rollbar where

import BasicPrelude
import Data.Aeson
import Data.Text (toLower)
import qualified Data.Vector as V
import Network.BSD (HostName)
import Data.Monoid (mappend)
import Control.Monad.Trans.Control (MonadBaseControl)
import Network.HTTP.Conduit
    ( RequestBody(RequestBodyLBS)
    , Request(method, requestBody)
    , parseUrl
    , withManager
    , http )

    
default (Text)

-- | report errors to rollbar.com and log them to stdout
reportErrorS :: (MonadIO m, MonadBaseControl IO m)
             => Text -- ^ environment (development, production, etc)
             -> HostName
             -> Text -- ^ log section
             -> Text -- ^ log message
             -> m ()
reportErrorS env hostName section msg =
    reportLoggerErrorS section hostName env logMessage msg
  where
    logMessage sec message = putStrLn $ "[Error#" `mappend` sec `mappend` "] " `mappend` " " `mappend` message

-- | used by Rollbar.MonadLogger to pass a custom logger
reportLoggerErrorS :: (MonadIO m, MonadBaseControl IO m)
                   => Text -- ^ environment (development, production, etc)
                   -> HostName
                   -> Text -- ^ log section
                   -> (Text -> Text -> m ()) -- ^ logger that takes the section and the message
                   -> Text -- ^ log message
                   -> m ()
reportLoggerErrorS env hostName section logger msg = do
    log msg
    liftIO $ do
      -- It would be more efficient to have the user setup the manager
      -- But reporting errors should be infrequent
      void $ withManager $ \manager -> do
          initReq <- liftIO $ parseUrl "https://api.rollbar.com/api/1/item/"
          let req = initReq { method = "POST", requestBody = RequestBodyLBS $ encode json }
          http req manager
    `catch` (\(e::SomeException) -> log $ show e)
  where
    log = logger section
    json = object
        [ "access_token" .= "8c0692277f2e4393bb6cf42f2eb617c0"
        , "data" .= object
            [ "environment" .= toLower env
            , "level"       .= "error"
            , "server"      .= object [ "host" .= hostName ]
            , "custom"      .= object []
            , "body"        .= object
                [ "trace" .= object
                    [ "frames" .= (Array $ V.fromList [])
                    , "exception" .= object ["class" .= section, "message" .= msg]
                    ]
                ]
            ]
        ]

