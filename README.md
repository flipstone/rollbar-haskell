rollbar-haskell
===============

send error notifications to rollbar.com

I have used a few different error notification services.

Rollbar has:

  * good support for client-side javascript errors
  * a nice UI 
  * a complete feature set (for me at least)


Usage
=====

Basic API

``` haskell
import Rollbar
reportError = reportErrorS token env hostName section err
main = liftIO $ reportError "oh no"
```

Use a custom logger in addition to sending information to rollbar

``` haskell
import Rollbar.MonadLogger
reportError = reportErrorS token env hostName section
main = liftIO $ reportError ($logErrorS) "oh no"
```

In the future we will add some nicer [monad-logger](http://hackage.haskell.org/package/monad-logger) integration. For now to get line number reporting in yoour logs in addition to the rollbar reporting, use ($logErrorS) at every reporting site.

We have some code like this to setup our environment and enable rollbar notification

``` haskell
setupNotifierS section = do
    hostName <- liftIO $ getHostName
    env <- runErrorT getAppEnv »= \case
      Left err -> do
        runStdoutLoggingT $ reportErrorS token "NONE" hostName section ($logErrorS) err 
        exitInt 1
      Right environ → return environ

    let reportError = reportErrorS token env hostName section
    let reportException logger = \(e :: SomeException) -> reportError logger (show e)
    let handleException logger = handle (λ(e :: SomeException) -> (reportErrorS token env hostName sectio  n logger (show e)) >> throwError (show e))
    return (handleException, reportException, reportError, env)
  where
    token = "my-rollbar-token"
```
