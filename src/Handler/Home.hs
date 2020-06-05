{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Html2Hamlet (transformHamlet)

getHomeR :: Handler Html
getHomeR = do
  (formWidget, formEnctype) <- generateFormPost fileForm
  let submission = Nothing :: Maybe Text
  defaultLayout $ do
    let (codeFormId, codeTextareaId, codeListId) = codeIds
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
    $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
  ((result, formWidget), formEnctype) <- runFormPost fileForm
  submission <- case result of
    FormSuccess fileInfo -> do
      content <- runConduit (fileSource fileInfo .| sinkLazy)
      return $ Just $ transformHamlet content
    _ -> return Nothing
  defaultLayout $ do
    let (codeFormId, codeTextareaId, codeListId) = codeIds
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
    $(widgetFile "homepage")

fileForm :: Form FileInfo
fileForm = renderBootstrap3 BootstrapBasicForm $
  fileAFormReq "Choose a file"
codeIds :: (Text, Text, Text)
codeIds = ("js-codeForm", "js-createCodeTextarea", "js-codeList")
