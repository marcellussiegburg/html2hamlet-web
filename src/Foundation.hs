{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Foundation where

import Import.NoFoundation
import Control.Monad.Logger    (LogSource)
import Text.Hamlet         (hamletFile)
import Text.Jasmine        (minifym)
import Yesod.Core.Types      (Logger)
import Yesod.Default.Util      (addStaticContentExternal)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

data Html2Hamlet = Html2Hamlet
  { appSettings    :: Html2HamletSettings
  , appStatic      :: Static
  , appHttpManager :: Manager
  , appLogger      :: Logger
  }

data MenuItem = MenuItem
  { menuItemLabel          :: Text
  , menuItemRoute          :: Route Html2Hamlet
  , menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

mkYesodData "Html2Hamlet" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor Html2Hamlet) (FormResult x, Widget)

instance Yesod Html2Hamlet where
  approot :: Approot Html2Hamlet
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

  makeSessionBackend :: Html2Hamlet -> IO (Maybe SessionBackend)
  makeSessionBackend _ = Just <$> defaultClientSessionBackend
    120  -- timeout in minutes
    "config/client_session_key.aes"

  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware = defaultYesodMiddleware

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage

    mcurrentRoute <- getCurrentRoute

    (title, parents) <- breadcrumbs

    let menuItems =
          [ NavbarLeft $ MenuItem
            { menuItemLabel = "Home"
            , menuItemRoute = HomeR
            , menuItemAccessCallback = True
            }
          ]

    let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
    let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

    let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR css_bootstrap_css
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  isAuthorized
    :: Route Html2Hamlet  -- ^ The route the user is visiting.
    -> Bool     -- ^ Whether or not this is a "write" request.
    -> Handler AuthResult
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized _ _ = return Authorized

  addStaticContent
    :: Text  -- ^ The file extension
    -> Text -- ^ The MIME content type
    -> LByteString -- ^ The contents of the file
    -> Handler (Maybe (Either Text (Route Html2Hamlet, [(Text, Text)])))
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      genFileName lbs = "autogen-" ++ base64md5 lbs

  shouldLogIO :: Html2Hamlet -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return $
    appShouldLogAll (appSettings app)
      || level == LevelWarn
      || level == LevelError

  makeLogger :: Html2Hamlet -> IO Logger
  makeLogger = return . appLogger

instance YesodBreadcrumbs Html2Hamlet where
  breadcrumb
    :: Route Html2Hamlet  -- ^ The route the user is visiting currently.
    -> Handler (Text, Maybe (Route Html2Hamlet))
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb  _ = return ("home", Nothing)

instance RenderMessage Html2Hamlet FormMessage where
  renderMessage :: Html2Hamlet -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

instance HasHttpManager Html2Hamlet where
  getHttpManager :: Html2Hamlet -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: Html2Hamlet -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
