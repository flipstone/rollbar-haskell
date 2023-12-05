{-# LANGUAGE CPP #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Main entry point to the application.
module Rollbar (
    ApiToken (..),
    UUID (..),
    Environment (..),
    Person (..),
    Settings (..),
    Options (..),
    emptyOptions,
    simpleLogMessage,
    reportErrorS,
    reportLoggerErrorS,
    reportErrorSCustomFingerprint,
    reportErrorSWithOptions,
    buildFrameJSON,
    buildJSON,
) where

import BasicPrelude
import Control.Exception.Lifted (catch)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson hiding (Options)
import Data.Aeson.TH hiding (Options)
import Data.Aeson.Types (parseMaybe)
import Data.Text (pack, toLower)
import qualified Data.Vector as V
import GHC.Stack (CallStack, SrcLoc (..), getCallStack)
import Network.BSD (HostName)
import Network.HTTP.Conduit (
    Request (method, requestBody),
    RequestBody (RequestBodyLBS),
    Response (..),
    httpLbs,
    newManager,
    parseUrlThrow,
    tlsManagerSettings,
 )

default (Text)

newtype ApiToken = ApiToken {unApiToken :: Text} deriving (Show)

newtype UUID = UUID {unUUID :: Text} deriving (Show)

-- (development, production, etc)
newtype Environment = Environment {unEnvironment :: Text} deriving (Show)

data Person = Person
    { id :: Text
    , username :: Maybe Text
    , email :: Maybe Text
    }
    deriving (Show)
deriveToJSON defaultOptions ''Person

data Settings = Settings
    { environment :: Environment
    , token :: ApiToken
    , hostName :: HostName
    , reportErrors :: Bool
    }
    deriving (Show)

data Options = Options
    { person :: Maybe Person
    , revisionSha :: Maybe Text
    }
    deriving (Show)

emptyOptions :: Options
emptyOptions = Options Nothing Nothing

simpleLogMessage :: (MonadIO m) => Text -> Text -> m ()
simpleLogMessage section message =
    putStrLn $ "[Error#" <> section <> "] " <> " " <> message

-- | report errors to rollbar.com and log them to stdout
reportErrorS ::
    (MonadIO m, MonadBaseControl IO m) =>
    Settings ->
    Options ->
    -- | log section
    Text ->
    Maybe CallStack ->
    -- | log message
    Text ->
    m ()
reportErrorS settings opts section =
    reportLoggerErrorS settings opts section simpleLogMessage

-- | used by Rollbar.MonadLogger to pass a custom logger
reportLoggerErrorS ::
    (MonadIO m, MonadBaseControl IO m) =>
    Settings ->
    Options ->
    -- | log section
    Text ->
    -- | logger that takes the section and the message
    (Text -> Text -> m ()) ->
    Maybe CallStack ->
    -- | log message
    Text ->
    m ()
reportLoggerErrorS settings opts section loggerS callstack msg =
    void $ reportErrorSWithOptions settings opts section (Just loggerS) msg Nothing callstack

-- | Pass in custom fingerprint for grouping on rollbar
reportErrorSCustomFingerprint ::
    (MonadIO m, MonadBaseControl IO m) =>
    Settings ->
    Options ->
    -- | log section
    Text ->
    -- | logger that takes the section and the message
    Maybe (Text -> Text -> m ()) ->
    Maybe CallStack ->
    -- | log message
    Text ->
    Text -> -- fingerprint
    m ()
reportErrorSCustomFingerprint settings opts section loggerS callstack msg fingerprint =
    void $ reportErrorSWithOptions settings opts section loggerS msg (Just fingerprint) callstack

-- | Pass in custom fingerprint for grouping on rollbar or a custom logger
reportErrorSWithOptions ::
    (MonadIO m, MonadBaseControl IO m) =>
    Settings ->
    Options ->
    -- | log section
    Text ->
    -- | logger that takes the section and the message
    Maybe (Text -> Text -> m ()) ->
    -- | log message
    Text ->
    Maybe Text -> -- fingerprint
    Maybe CallStack ->
    m (Maybe UUID)
reportErrorSWithOptions settings opts section loggerS msg fingerprint callstack =
    if reportErrors settings
        then go
        else pure Nothing
  where
    go =
        do
            logger msg
            liftIO $ do
                initReq <- parseUrlThrow "https://api.rollbar.com/api/1/item/"
                manager <- newManager tlsManagerSettings
                let req = initReq{method = "POST", requestBody = RequestBodyLBS $ encode rollbarJson}
                response <- httpLbs req manager
                let body = responseBody response
                    uuid =
                        fmap UUID
                            $ parseMaybe
                                ( \obj -> do
                                    result <- obj .: "result"
                                    result .: "uuid"
                                )
                            =<< decode body
                pure uuid
            `catch` (\(e :: SomeException) -> Nothing <$ logger (pack $ show e))

    logger = fromMaybe defaultLogger loggerS section
    defaultLogger message = pure $ simpleLogMessage section message
    rollbarJson = buildJSON settings opts section msg fingerprint callstack

buildFrameJSON :: (String, SrcLoc) -> Value
buildFrameJSON (name, srcLoc) =
    object
        [ "filename" .= String (pack $ srcLocFile srcLoc)
        , "method" .= String (pack name)
        , "lineno" .= toJSON (srcLocStartLine srcLoc)
        , "colno" .= toJSON (srcLocStartCol srcLoc)
        , "class_name" .= String (pack $ srcLocModule srcLoc)
        ]

buildJSON ::
    Settings ->
    Options ->
    -- | log section
    Text ->
    -- | log message
    Text ->
    -- | fingerprint
    Maybe Text ->
    Maybe CallStack ->
    Value
buildJSON settings opts section msg fingerprint callstack =
    object
        [ "access_token" .= unApiToken (token settings)
        , "data"
            .= object
                ( [ "environment" .= toLower (unEnvironment $ environment settings)
                  , "level" .= ("error" :: Text)
                  , "server" .= object ["host" .= hostName settings, "sha" .= revisionSha opts]
                  , "person" .= toJSON (person opts)
                  , "body"
                        .= object
                            [ "trace"
                                .= object
                                    [ "frames" .= Array (V.fromList $ maybe [] (map buildFrameJSON . getCallStack) callstack)
                                    , "exception" .= object ["class" .= section, "message" .= msg]
                                    ]
                            ]
                  ]
                    ++ fp
                )
        , "title" .= title
        , "notifier"
            .= object
                [ "name" .= "rollbar-haskell"
                , "version" .= "1.1.3"
                ]
        ]
  where
    title = section <> ": " <> msg
    fp =
        case fingerprint of
            Just fp' ->
                ["fingerprint" .= fp']
            Nothing ->
                []
