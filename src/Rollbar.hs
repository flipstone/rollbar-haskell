{-# Language NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Main entry point to the application.
module Rollbar where

import BasicPrelude
import Data.Aeson
import Data.Text (toLower)
import qualified Data.Vector as V
import Network.BSD (HostName)
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
             => Text -- ^ access token
             -> Text -- ^ environment (development, production, etc)
             -> HostName
             -> Text -- ^ log section
             -> Text -- ^ log message
             -> m ()
reportErrorS token env hostName section =
    reportLoggerErrorS token env hostName section logMessage
  where
    logMessage sec message = putStrLn $ "[Error#" `mappend` sec `mappend` "] " `mappend` " " `mappend` message

-- | used by Rollbar.MonadLogger to pass a custom logger
reportLoggerErrorS :: (MonadIO m, MonadBaseControl IO m)
                   => Text -- ^ access token
                   -> Text -- ^ environment (development, production, etc)
                   -> HostName
                   -> Text -- ^ log section
                   -> (Text -> Text -> m ()) -- ^ logger that takes the section and the message
                   -> Text -- ^ log message
                   -> m ()
reportLoggerErrorS token env hostName section loggerS msg = do
    logger msg
    liftIO $
      -- It would be more efficient to have the user setup the manager
      -- But reporting errors should be infrequent
      void $ withManager $ \manager -> do
          initReq <- liftIO $ parseUrl "https://api.rollbar.com/api/1/item/"
          let req = initReq { method = "POST", requestBody = RequestBodyLBS $ encode rollbarJson }
          http req manager
    `catch` (\(e::SomeException) -> logger $ show e)
  where
    logger = loggerS section
    rollbarJson = object
        [ "access_token" .= token
        , "data" .= object
            [ "environment" .= toLower env
            , "level"       .= ("error" :: Text)
            , "server"      .= object [ "host" .= hostName ]
            , "custom"      .= object []
            , "body"        .= object
                [ "trace" .= object
                    [ "frames" .= (Array $ V.fromList [])
                    , "exception" .= object ["class" .= section, "message" .= msg]
                    ]
                ]
            ]
        , "title" .= msg
        , "notifier" .= object [
            "name"    .= "rollbar-haskell"
          , "version" .= "0.2.1"
          ]
        ]

