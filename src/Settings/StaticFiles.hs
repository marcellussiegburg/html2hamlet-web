{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Settings.StaticFiles where

import Settings     (appStaticDir, compileTimeHtml2HamletSettings)
import Yesod.Static (staticFiles)

staticFiles (appStaticDir compileTimeHtml2HamletSettings)
