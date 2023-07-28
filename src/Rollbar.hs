{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Main entry point to the application.
module Rollbar
  ( ApiToken (..)
  , Environment (..)
  , Person (..)
  , Settings (..)
  , Options (..)
  , emptyOptions
  , reportErrorS
  , reportLoggerErrorS
  , reportErrorSCustomFingerprint
  ) where

import qualified Control.Exception as Ex
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MIO
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified GHC.Stack as Stack
import Network.BSD (HostName)
import Network.HTTP.Conduit
  ( Request (method, requestBody)
  , RequestBody (RequestBodyLBS)
  , http
  , newManager
  , parseUrlThrow
  , tlsManagerSettings
  )

newtype ApiToken = ApiToken {unApiToken :: T.Text}

-- (development, production, etc)
newtype Environment = Environment {unEnvironment :: T.Text}

data Person = Person
  { personId :: T.Text
  , personUsername :: Maybe T.Text
  , personEmail :: Maybe T.Text
  }

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
  , hostName :: HostName
  , reportErrors :: Bool
  }

data Options = Options
  { optionsPerson :: Maybe Person
  , optionsRevisionSha :: Maybe T.Text
  }

emptyOptions :: Options
emptyOptions = Options Nothing Nothing

-- | report errors to rollbar.com and log them to stdout
reportErrorS ::
  Settings ->
  Options ->
  -- | log section
  T.Text ->
  Maybe Stack.CallStack ->
  -- | log message
  T.Text ->
  IO ()
reportErrorS settings opts section =
  let
    logMessage sec message =
      putStrLn . T.unpack $ "[Error#" <> sec <> "] " <> " " <> message
  in
    reportLoggerErrorS settings opts section logMessage

-- | used by Rollbar.MonadLogger to pass a custom logger
reportLoggerErrorS ::
  Settings ->
  Options ->
  -- | log section
  T.Text ->
  -- | logger that takes the section and the message
  (T.Text -> T.Text -> IO ()) ->
  Maybe Stack.CallStack ->
  -- | log message
  T.Text ->
  IO ()
reportLoggerErrorS settings opts section loggerS callstack msg =
  if reportErrors settings
    then go
    else return ()
 where
  go =
    do
      logger msg
      MIO.liftIO $ do
        -- It would be more efficient to have the user setup the manager
        -- But reporting errors should be infrequent

        initReq <- parseUrlThrow "https://api.rollbar.com/api/1/item/"
        manager <- newManager tlsManagerSettings
        let
          req = initReq {method = "POST", requestBody = RequestBodyLBS $ Aeson.encode rollbarJson}
        runResourceT $ Monad.void $ http req manager
      `Ex.catch` (\(e :: Ex.SomeException) -> logger $ T.pack $ show e)

  logger = loggerS section
  rollbarJson = buildJSON settings opts section msg Nothing callstack

-- | Pass in custom fingerprint for grouping on rollbar
reportErrorSCustomFingerprint ::
  Settings ->
  Options ->
  -- | log section
  T.Text ->
  -- | logger that takes the section and the message
  Maybe (T.Text -> T.Text -> IO ()) ->
  -- | log message
  T.Text ->
  T.Text -> -- fingerprint
  Maybe Stack.CallStack ->
  IO ()
reportErrorSCustomFingerprint settings opts section loggerS msg fingerprint callstack =
  if reportErrors settings
    then go
    else return ()
 where
  go =
    do
      logger msg
      MIO.liftIO $ do
        initReq <- parseUrlThrow "https://api.rollbar.com/api/1/item/"
        manager <- newManager tlsManagerSettings
        let
          req = initReq {method = "POST", requestBody = RequestBodyLBS $ Aeson.encode rollbarJson}
        runResourceT $ Monad.void $ http req manager
      `Ex.catch` (\(e :: Ex.SomeException) -> logger $ T.pack $ show e)

  logger = Maybe.fromMaybe defaultLogger loggerS section
  defaultLogger message = pure $ putStrLn . T.unpack $ "[Error#" <> section <> "] " <> " " <> message
  rollbarJson = buildJSON settings opts section msg (Just fingerprint) callstack

buildFrameJSON :: (String, Stack.SrcLoc) -> Aeson.Value
buildFrameJSON (name, srcLoc) =
  Aeson.object
    [ "filename" .= Aeson.String (T.pack $ Stack.srcLocFile srcLoc)
    , "method" .= Aeson.String (T.pack name)
    , "lineno" .= Aeson.toJSON (Stack.srcLocStartLine srcLoc)
    , "colno" .= Aeson.toJSON (Stack.srcLocStartCol srcLoc)
    , "class_name" .= Aeson.String (T.pack $ Stack.srcLocModule srcLoc)
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
  Maybe Stack.CallStack ->
  Aeson.Value
buildJSON settings opts section msg fingerprint callstack =
  Aeson.object
    [ "access_token" .= unApiToken (token settings)
    , "data"
        .= Aeson.object
          ( [ "environment" .= T.toLower (unEnvironment $ environment settings)
            , "level" .= ("error" :: T.Text)
            , "server" .= Aeson.object ["host" .= hostName settings, "sha" .= optionsRevisionSha opts]
            , "person" .= Aeson.toJSON (optionsPerson opts)
            , "body"
                .= Aeson.object
                  [ "trace"
                      .= Aeson.object
                        [ "frames" .= Aeson.Array (V.fromList $ maybe [] (map buildFrameJSON . Stack.getCallStack) callstack)
                        , "exception" .= Aeson.object ["class" .= section, "message" .= msg]
                        ]
                  ]
            ]
              ++ fp
          )
    , "title" .= title
    , "notifier"
        .= Aeson.object
          [ "name" .= ("rollbar-haskell" :: T.Text)
          , "version" .= ("1.1.3" :: T.Text)
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
