{-# Language NoImplicitPrelude, FlexibleContexts, OverloadedStrings, ExtendedDefaultRules #-}
-- | Main entry point to the application.
module Rollbar where

import Language.Haskell.TH.Syntax
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



reportPrintErrorS :: (MonadIO m, MonadBaseControl IO m) => Text -> HostName -> Text -> Text -> m ()
reportPrintErrorS section hostName env msg = liftIO $ do
    logMessage msg
    -- It would be more efficient to have the user setup the manager
    -- But reporting errors should be infrequent
    void $ withManager $ \manager -> do
        initReq <- liftIO $ parseUrl "https://api.rollbar.com/api/1/item/"
        let req = initReq { method = "POST", requestBody = RequestBodyLBS $ encode json }
        http req manager
    `catchany` (\_ -> logMessage (show msg))
  where
    logMessage message = putStrLn $ "[Error#" `mappend` section `mappend` "] " `mappend` " " `mappend` message
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

-- | A helper to catch any exception
-- (same as @... `catch` \(e :: SomeException) -> ...@).
catchany :: IO a -> (SomeException -> IO a) -> IO a
catchany = catch