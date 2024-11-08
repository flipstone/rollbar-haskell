{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point to the application.
module Rollbar
  ( ApiToken (..)
  , UUID (..)
  , Environment (..)
  , Person (..)
  , Settings (..)
  , Options (..)
  , ErrorLevel (..)
  , emptyOptions
  , simpleLogMessage
  , reportErrorS
  , reportLoggerErrorS
  , reportErrorSCustomFingerprint
  , reportErrorSWithOptions
  , buildFrameJSON
  , buildJSON
  ) where

import Control.Exception qualified as Ex
import Control.Exception.Lifted (catch)
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MIO
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as Enc
import Data.Vector qualified as V
import GHC.Stack (CallStack, SrcLoc (..), getCallStack)
import Network.HTTP.Conduit
  ( Request (method, requestBody)
  , RequestBody (RequestBodyLBS)
  , Response (..)
  , httpLbs
  , newManager
  , parseUrlThrow
  , requestHeaders
  , tlsManagerSettings
  )

newtype ApiToken = ApiToken {unApiToken :: T.Text} deriving (Show)

newtype UUID = UUID {unUUID :: T.Text} deriving (Show)

-- (development, production, etc)
newtype Environment = Environment {unEnvironment :: T.Text} deriving (Show)

data Person = Person
  { personId :: T.Text
  , personUsername :: Maybe T.Text
  , personEmail :: Maybe T.Text
  }
  deriving (Show)

instance Aeson.ToJSON Person where
  toJSON person =
    Aeson.object
      [ "id" .= personId person
      , "username" .= personUsername person
      , "email" .= personEmail person
      ]

data Settings = Settings
  { environment :: Environment
  , token :: ApiToken
  , hostName :: String
  , reportErrors :: Bool
  }

data Options = Options
  { optionsPerson :: Maybe Person
  , optionsRevisionSha :: Maybe T.Text
  }
  deriving (Show)

data ErrorLevel
  = Critical
  | Error
  | Warning
  | Info
  | Debug

instance Aeson.ToJSON ErrorLevel where
  toJSON lvl =
    Aeson.String $
      case lvl of
        Critical -> "critical"
        Error -> "error"
        Warning -> "warning"
        Info -> "info"
        Debug -> "debug"

emptyOptions :: Options
emptyOptions = Options Nothing Nothing

simpleLogMessage :: MIO.MonadIO m => T.Text -> T.Text -> m ()
simpleLogMessage section message =
  MIO.liftIO $ putStrLn $ T.unpack $ "[Error#" <> section <> "] " <> " " <> message

-- | report errors to rollbar.com and log them to stdout
reportErrorS ::
  (MIO.MonadIO m, MonadBaseControl IO m) =>
  Settings ->
  Options ->
  -- | log section
  T.Text ->
  Maybe CallStack ->
  ErrorLevel ->
  -- | log message
  T.Text ->
  m ()
reportErrorS settings opts section =
  reportLoggerErrorS settings opts section simpleLogMessage

-- | used by Rollbar.MonadLogger to pass a custom logger
reportLoggerErrorS ::
  (MIO.MonadIO m, MonadBaseControl IO m) =>
  Settings ->
  Options ->
  -- | log section
  T.Text ->
  -- | logger that takes the section and the message
  (T.Text -> T.Text -> m ()) ->
  Maybe CallStack ->
  ErrorLevel ->
  -- | log message
  T.Text ->
  m ()
reportLoggerErrorS settings opts section loggerS callstack errorLevel msg =
  Monad.void $ reportErrorSWithOptions settings opts section (Just loggerS) msg Nothing callstack errorLevel

-- | Pass in custom fingerprint for grouping on rollbar
reportErrorSCustomFingerprint ::
  (MIO.MonadIO m, MonadBaseControl IO m) =>
  Settings ->
  Options ->
  -- | log section
  T.Text ->
  -- | logger that takes the section and the message
  Maybe (T.Text -> T.Text -> m ()) ->
  Maybe CallStack ->
  ErrorLevel ->
  -- | log message
  T.Text ->
  T.Text -> -- fingerprint
  m ()
reportErrorSCustomFingerprint settings opts section loggerS callstack errorLevel msg fingerprint =
  Monad.void $ reportErrorSWithOptions settings opts section loggerS msg (Just fingerprint) callstack errorLevel

-- | Pass in custom fingerprint for grouping on rollbar or a custom logger
reportErrorSWithOptions ::
  (MIO.MonadIO m, MonadBaseControl IO m) =>
  Settings ->
  Options ->
  -- | log section
  T.Text ->
  -- | logger that takes the section and the message
  Maybe (T.Text -> T.Text -> m ()) ->
  -- | log message
  T.Text ->
  Maybe T.Text -> -- fingerprint
  Maybe CallStack ->
  ErrorLevel ->
  m (Maybe UUID)
reportErrorSWithOptions settings opts section loggerS msg fingerprint callstack errorLevel =
  if reportErrors settings
    then go
    else pure Nothing
 where
  go =
    do
      logger msg
      MIO.liftIO $ do
        unauthenticatedReq <- parseUrlThrow "https://api.rollbar.com/api/1/item/"
        manager <- newManager tlsManagerSettings
        let
          authenticatedRequest =
            unauthenticatedReq
              { method = "POST"
              , requestHeaders = [("X-Rollbar-Access-Token", Enc.encodeUtf8 . unApiToken $ token settings)]
              , requestBody = RequestBodyLBS $ Aeson.encode rollbarJson
              }
        response <- httpLbs authenticatedRequest manager
        let
          body = responseBody response
          uuid =
            fmap UUID $
              parseMaybe
                ( \obj -> do
                    result <- obj .: "result"
                    result .: "uuid"
                )
                =<< Aeson.decode body
        pure uuid
      `catch` (\(e :: Ex.SomeException) -> Nothing <$ logger (T.pack $ show e))

  logger = Maybe.fromMaybe defaultLogger loggerS section
  defaultLogger message = pure $ simpleLogMessage section message
  rollbarJson = buildJSON settings opts section msg fingerprint callstack errorLevel

buildFrameJSON :: (String, SrcLoc) -> Aeson.Value
buildFrameJSON (name, srcLoc) =
  Aeson.object
    [ "filename" .= Aeson.String (T.pack $ srcLocFile srcLoc)
    , "method" .= Aeson.String (T.pack name)
    , "lineno" .= Aeson.toJSON (srcLocStartLine srcLoc)
    , "colno" .= Aeson.toJSON (srcLocStartCol srcLoc)
    , "class_name" .= Aeson.String (T.pack $ srcLocModule srcLoc)
    ]

buildJSON ::
  Settings ->
  Options ->
  -- | log section
  T.Text ->
  -- | log message
  T.Text ->
  -- | fingerprint
  Maybe T.Text ->
  Maybe CallStack ->
  ErrorLevel ->
  Aeson.Value
buildJSON settings opts section msg fingerprint callstack level =
  Aeson.object
    [ "data"
        .= Aeson.object
          ( [ "environment" .= T.toLower (unEnvironment $ environment settings)
            , "level" .= Aeson.toJSON level
            , "code_version" .= optionsRevisionSha opts
            , "language" .= ("haskell" :: T.Text)
            , "server" .= Aeson.object ["host" .= hostName settings]
            , "person" .= Aeson.toJSON (optionsPerson opts)
            , "body"
                .= Aeson.object
                  [ "trace"
                      .= Aeson.object
                        [ "frames" .= Aeson.Array (V.fromList $ maybe [] (map buildFrameJSON . getCallStack) callstack)
                        , "exception" .= Aeson.object ["class" .= section, "message" .= msg]
                        ]
                  ]
            ]
              <> fp
          )
    , "title" .= title
    , "notifier"
        .= Aeson.object
          [ "name" .= ("rollbar-haskell" :: T.Text)
          , "version" .= ("2.2.0" :: T.Text)
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
