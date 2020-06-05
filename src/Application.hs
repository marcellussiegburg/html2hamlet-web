{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
  ( getApplicationDev
  , appMain
  , develMain
  , makeFoundation
  , makeLogWare
  -- * for DevelMain
  , getApplicationRepl
  , shutdownHtml2Hamlet
  -- * for GHCI
  , handler
  ) where

import Control.Monad.Logger             (liftLoc)
import Import
import Language.Haskell.TH.Syntax       (qLocation)
import Network.HTTP.Client.TLS          (getGlobalManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (
  Settings, defaultSettings, defaultShouldDisplayException,
  runSettings, setHost, setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (
  Destination (Logger), IPAddrSource (..),
  OutputFormat (..), destination,
  mkRequestLogger, outputFormat)
import System.Log.FastLogger (
  defaultBufSize, newStdoutLoggerSet, toLogStr)

import Handler.Code
import Handler.Common
import Handler.Home

mkYesodDispatch "Html2Hamlet" resourcesHtml2Hamlet

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: Html2HamletSettings -> IO Html2Hamlet
makeFoundation appSettings = do
  appHttpManager <- getGlobalManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <-
    (if appMutableStatic appSettings then staticDevel else static)
    (appStaticDir appSettings)

  return Html2Hamlet {..}

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: Html2Hamlet -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  appPlain <- toWaiAppPlain foundation
  return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: Html2Hamlet -> IO Middleware
makeLogWare foundation =
  mkRequestLogger def
    { outputFormat =
        if appDetailedRequestLogging $ appSettings foundation
        then Detailed True
        else Apache
             (if appIpFromHeader $ appSettings foundation
              then FromFallback
              else FromSocket)
    , destination = Logger $ loggerSet $ appLogger foundation
    }


-- | Warp settings for the given foundation value.
warpSettings :: Html2Hamlet -> Settings
warpSettings foundation =
    setPort (appPort $ appSettings foundation)
  $ setHost (appHost $ appSettings foundation)
  $ setOnException (\_req e ->
    when (defaultShouldDisplayException e) $ messageLoggerSource
      foundation
      (appLogger foundation)
      $(qLocation >>= liftLoc)
      "yesod"
      LevelError
      (toLogStr $ "Exception from Warp: " ++ show e))
    defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  settings <- getHtml2HamletSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app <- makeApplication foundation
  return (wsettings, app)

getHtml2HamletSettings :: IO Html2HamletSettings
getHtml2HamletSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  settings <- loadYamlSettingsArgs
    [configSettingsYmlValue]
    useEnv

  foundation <- makeFoundation settings

  app <- makeApplication foundation

  runSettings (warpSettings foundation) app

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, Html2Hamlet, Application)
getApplicationRepl = do
  settings <- getHtml2HamletSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app1 <- makeApplication foundation
  return (getPort wsettings, foundation, app1)

shutdownHtml2Hamlet :: Html2Hamlet -> IO ()
shutdownHtml2Hamlet _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getHtml2HamletSettings >>= makeFoundation >>= flip unsafeHandler h
