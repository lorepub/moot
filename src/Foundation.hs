{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foundation where

import Import.NoFoundation

import Control.Monad.Logger (LogSource)
import Database.Persist.Sql (runSqlPool)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Jasmine         (minifym)
import Yesod.Core.Types     (Logger)
import Yesod.Default.Util   (addStaticContentExternal)
import qualified Yesod.Core.Unsafe as Unsafe

import AppType
import Routes

type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

instance Yesod App where
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout w = do
        p <- widgetToPageContent w
        msgs <- getMessages
        let pt = pageTitle p
            title = case renderHtml pt of
                      "" -> "Moot"
                      t -> "Moot - " <> t
        withUrlRenderer [hamlet|
            $newline never
            $doctype 5
            <html>
                <head>
                    <title>#{title}
                    ^{pageHead p}
                <body>
                    $forall (status, msg) <- msgs
                        <p class="message #{status}">#{msg}
                    ^{pageBody p}
            |]

    yesodMiddleware :: ToTypedContent res
                    => Handler res
                    -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    isAuthorized _ _ = return Authorized

    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
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

    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        app <- getYesod
        runSqlPool action $ appConnPool app

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
