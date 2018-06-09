module Handler.Sessions where

import Import.NoFoundation
import Control.Monad.Trans.Maybe
import Data.Time.Clock (addUTCTime)

type YesodLog site = (Yesod site)

userSess :: Text
userSess = "user_id"

rememberSess :: Text
rememberSess = "remember_me"

timestampSess :: Text
timestampSess = "time_created"

setUserSession :: (YesodLog site)
               => Key User
               -> Bool
               -> HandlerFor site ()
setUserSession keyUser rememberMe = do
  setSession userSess (toPathPiece keyUser)
  if rememberMe
    then setSession rememberSess "True"
    else deleteSession rememberSess
  t <- liftIO getCurrentTime
  setSession timestampSess (toPathPiece (SessionTime t))
  return ()

newtype SessionTime =
    SessionTime UTCTime
     deriving (Eq, Read, Show)

instance PathPiece SessionTime where
    fromPathPiece = readMay . unpack
    toPathPiece = tshow

sessionTooOld
    :: (YesodLog site)
    => UTCTime -> HandlerFor site Bool
sessionTooOld currentTime = do
    remember <- getSessionKey rememberSess
    timestamp <- getSessionKey timestampSess
    case timestamp >>= (fromPathPiece . decodeUtf8) of
        Nothing -> return True
        (Just (SessionTime t))
        -- no remember flag, so should only last 2 hours
         -> do
            let shortLife = 60 * 60 * 2
                -- shortLife = 5 -- for testing
                -- there was a remember flag, so we give it 1 month
                longLife = 60 * 60 * 24 * 30 * 1
                secondsToLast = maybe shortLife (const longLife) remember
                deadline = addUTCTime secondsToLast t
            -- if currentTime is greater than the
            -- session deadline, it's too old.
            return (currentTime > deadline)

sessionMiddleware
    :: (YesodLog site)
    => HandlerFor site resp -> HandlerFor site resp
sessionMiddleware handler = do
    t <- liftIO getCurrentTime
    tooOld <- sessionTooOld t
    if tooOld
        then deleteLoginData >> handler
        else handler

getSessionKey
    :: Text
    -> YesodLog site =>
       HandlerFor site (Maybe ByteString)
getSessionKey k = do
    sess <- getSession
    return $ lookup k sess

getSessionUserK
    :: YesodLog site
    => HandlerFor site (Maybe ByteString)
getSessionUserK = getSessionKey userSess

handleDumpSessionR
    :: YesodLog site
    => HandlerFor site Text
handleDumpSessionR =
  tshow <$> getSession

deleteLoginData
    :: YesodLog site
    => HandlerFor site ()
deleteLoginData = do
  deleteSession userSess
  deleteSession rememberSess
  deleteSession timestampSess

getUserKey
    :: (YesodLog site)
    => HandlerFor site (Maybe (Key User))
getUserKey =
    runMaybeT $ do
       userId <- MaybeT getSessionUserK
       userInt <- justZ $ fromPathPiece (decodeUtf8 userId)
       return (toSqlKey userInt)

getUser
    :: ( YesodLog site
       , YesodPersist site
       , YesodPersistBackend site ~ SqlBackend
       )
    => HandlerFor site (Maybe (Entity User))
getUser =
    runMaybeT $ do
       userKey <- MaybeT $ getUserKey
       user <- MaybeT $ runDB $ get userKey
       return $ Entity userKey user
