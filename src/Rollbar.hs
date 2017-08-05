{-# Language TemplateHaskell, NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Main entry point to the application.
module Rollbar where

import BasicPrelude
import Data.Aeson
import Data.Aeson.TH hiding (Options)
import Data.Text (toLower, pack)
import qualified Data.Vector as V
import Network.BSD (HostName)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (runResourceT)
import Network.HTTP.Conduit
    ( RequestBody(RequestBodyLBS)
    , Request(method, requestBody)
    , parseUrlThrow
    , newManager
    , tlsManagerSettings
    , http )

default (Text)

newtype ApiToken = ApiToken { unApiToken :: Text } deriving Show

-- (development, production, etc)
newtype Environment = Environment { unEnvironment :: Text } deriving Show

data Person = Person
               { id       :: Text
               , username :: Maybe Text
               , email    :: Maybe Text
               } deriving Show
deriveToJSON defaultOptions ''Person

data Settings = Settings
                  { environment :: Environment
                  , token       :: ApiToken
                  , hostName    :: HostName
                  } deriving Show

data Options = Options
       { person   :: Maybe Person
       , revisionSha :: Maybe Text
       } deriving Show

emptyOptions :: Options
emptyOptions = Options Nothing Nothing

-- | report errors to rollbar.com and log them to stdout
reportErrorS :: (MonadIO m, MonadBaseControl IO m)
             => Settings
             -> Options
             -> Text -- ^ log section
             -> Text -- ^ log message
             -> m ()
reportErrorS settings opts section =
    reportLoggerErrorS settings opts section logMessage
  where
    logMessage sec message = putStrLn $ "[Error#" `mappend` sec `mappend` "] " `mappend` " " `mappend` message

-- | used by Rollbar.MonadLogger to pass a custom logger
reportLoggerErrorS :: (MonadIO m, MonadBaseControl IO m)
                   => Settings
                   -> Options
                   -> Text -- ^ log section
                   -> (Text -> Text -> m ()) -- ^ logger that takes the section and the message
                   -> Text -- ^ log message
                   -> m ()
reportLoggerErrorS settings opts section loggerS msg = do
    logger msg
    liftIO $ do
      -- It would be more efficient to have the user setup the manager
      -- But reporting errors should be infrequent

      initReq <- parseUrlThrow "https://api.rollbar.com/api/1/item/"
      manager <- newManager tlsManagerSettings
      let req = initReq { method = "POST", requestBody = RequestBodyLBS $ encode rollbarJson }
      runResourceT $ void $ http req manager
    `catch` (\(e::SomeException) -> logger $ pack $ show e)
  where
    title = section <> ": " <> msg
    logger = loggerS section
    rollbarJson = object
        [ "access_token" .= unApiToken (token settings)
        , "data" .= object
            [ "environment" .= toLower (unEnvironment $ environment settings)
            , "level"       .= ("error" :: Text)
            , "server"      .= object [ "host" .= hostName settings, "sha" .= revisionSha opts]
            , "person"      .= toJSON (person opts)
            -- , "custom"      .= object []
            , "body"        .= object
                [ "trace" .= object
                    [ "frames" .= (Array $ V.fromList [])
                    , "exception" .= object ["class" .= section, "message" .= msg]
                    ]
                ]
            ]
        , "title" .= title
        , "notifier" .= object [
            "name"    .= "rollbar-haskell"
          , "version" .= "0.2.1"
          ]
        ]
