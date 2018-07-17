module Handler.Auth where

import Import

import Handler.Auth.Forms
import Handler.Auth.Views
import Handler.Sessions
import Helpers.Forms
import Helpers.Views

redirectIfLoggedIn :: (RedirectUrl App r) => r -> Handler ()
redirectIfLoggedIn r = do
  maybeUser <- getUser
  case maybeUser of
    Nothing -> return ()
    (Just _) -> redirect r

requireUser :: Handler (Entity User)
requireUser = do
  maybeUser <- getUser
  case maybeUser of
    Nothing -> do
      $logWarn "No user session active"
      redirect HomeR -- LoginR
    (Just user) -> return user

requireOwner :: Handler (Entity User, Entity Owner)
requireOwner = do
  user <- requireUser
  maybeOwner <- runDB $ getOwnerForUser (entityKey user)
  case maybeOwner of
    Nothing -> do
      $logWarn "Current owner is not associated with an account"
      permissionDenied "You are not an owner"
    (Just owner) ->
      return (user, owner)

requireAccount :: Handler (Entity User, Entity Owner, Entity Account)
requireAccount = do
  (user, owner) <- requireOwner
  maybeAccount <- runDB $ getAccountForOwner (entityKey owner)
  case maybeAccount of
    Nothing -> do
      $logWarn "Current user is not associated with an account"
      permissionDenied "No accounts associated with your user"
    Just account -> return (user, owner, account)

requireOwnerForEntity ::
     (DBVal a)
  => EntityField a (Key Owner)
  -> (SqlExpr (Entity a) -> SqlQuery b)
  -> Handler ( Entity User
             , Entity Owner
             , Entity a
             )
requireOwnerForEntity f w = do
  (user, owner) <- requireOwner
  maybeRec <- runDB $ getRecByField' f w (entityKey owner)
  case maybeRec of
    Nothing -> do
      $logWarn "Current user is not Owner for requested resource"
      permissionDenied "You are not an owner for this resource"
    (Just rec') ->
      return (user, owner, rec')

requireOwnerForConference ::
     ConferenceId
  -> Handler ( Entity User
             , Entity Owner
             , Entity Account
             , Entity Conference
             )
requireOwnerForConference conferenceId = do
  (user, owner) <- requireOwner
  maybeConfAcc <- runDB $ getAccAndConf owner
  case maybeConfAcc of
    Nothing -> permissionDenied "You are not the owner for this conference"
    (Just (acc, conf)) ->
      return (user, owner, acc, conf)
  where
    getAccAndConf owner =
      selectFirst $
        from $ \(conference `InnerJoin` account) -> do
          on (conference ^. ConferenceAccount ==. account ^. AccountId)
          where_ (account ^. AccountOwner ==. val (entityKey owner))
          return (account, conference)

-- data AdminOrStronger =
--     AOSA Admin
--   | AOSO Owner
--   deriving Show

-- data Permissions =
--   CanEditAbstractTypes AdminOrStronger
--   deriving Show

-- type family Permitted a where
--   Permitted 'CanEditAbstractTypes = Admin

getLoginR :: Handler Html
getLoginR = do
  redirectIfLoggedIn HomeR
  (loginFormWidget, _) <- generateFormPost loginForm
  renderLogin loginFormWidget []

postLoginR :: Handler Html
postLoginR = do
  redirectIfLoggedIn HomeR
  ((result, widget), _) <- runFormPost loginForm
  case result of
    FormSuccess (email, password) -> do
      maybeUP <- runDB (getUserPassword email)
      case maybeUP of
        Nothing ->
          renderLogin widget ["Email not yet registered"]
        (Just
         ( Entity dbUserKey _
         , Entity _ Password{..})) -> do
          let success =
                passwordMatches passwordHash password
          case success of
            False ->
              renderLogin widget ["Password did not match"]
            True -> do
              setUserSession dbUserKey True
              redirect HomeR
    _ -> renderLogin widget ["Form failed validation"]

getSignupR :: Handler Html
getSignupR = do
  redirectIfLoggedIn HomeR
  (signupFormWidget, _) <- generateFormPost signupForm
  renderSignup signupFormWidget []

postSignupR :: Handler Html
postSignupR = do
  redirectIfLoggedIn HomeR
  ((result, widget), _) <- runFormPost signupForm
  case result of
    FormSuccess SignupForm{..} -> do
      dbUserKeyM <- runDB $ do
        maybeUP <- getUserByEmail signupEmail
        case maybeUP of
          (Just _) ->
            return Nothing
          Nothing -> do
            (Entity dbUserKey _) <-
              createUser signupEmail signupPassword
            return (Just dbUserKey)
      case dbUserKeyM of
        Nothing -> do
          renderSignup widget ["User already exists."]
        (Just dbUserKey) -> do
          setUserSession dbUserKey True
          redirect HomeR
    _ -> renderSignup widget []

getSignoutR :: Handler Html
getSignoutR = do
  deleteLoginData
  redirect HomeR

getForgotR :: Handler Html
getForgotR = do
  redirectIfLoggedIn HomeR
  (forgotFormWidget, _) <- generateFormPost forgotForm
  renderForgot forgotFormWidget []

postForgotR :: Handler Html
postForgotR = do
  redirectIfLoggedIn HomeR
  ((result, widget), _) <- runFormPost forgotForm
  case result of
    FormSuccess email -> do
      maybeUP <- runDB $ getUserByEmail email
      case maybeUP of
        (Just (Entity userKey _)) -> do
          (Entity _ (Reset token _ _)) <- runDB $ do
            deleteExistingResets userKey
            createReset userKey
          -- send link in email
          $logInfo $ tokenText token
          renderNotice "Success" ["Please check your email to reset your password."]
        Nothing -> do
          renderForgot widget ["This user does not exist."]
    _ -> renderForgot widget []

getResetR :: Token -> Handler Html
getResetR token = do
  redirectIfLoggedIn HomeR
  maybeUser <- runDB $ do
    deleteOldResets
    getUserByResetToken token
  case maybeUser of
    (Just _) -> do
      (resetFormWidget, _) <- generateFormPost resetForm
      renderReset resetFormWidget token []
    Nothing ->
      redirect HomeR

postResetR :: Token -> Handler Html
postResetR token = do
  redirectIfLoggedIn HomeR
  maybeUserPassword <- runDB $ do
    deleteOldResets
    getUserPasswordByResetToken token
  case maybeUserPassword of
    (Just _) -> do
      ((result, _), _) <- runFormPost resetForm
      case result of
        FormSuccess resetFormData -> do
          if resetPassword resetFormData == resetConfirm resetFormData
          then do
            runDB $ resetUserPassword token (resetPassword resetFormData)
            (loginFormWidget, _) <- generateFormPost loginForm
            renderLogin loginFormWidget []
          else do
            (resetFormWidget, _) <- generateFormPost resetForm
            renderReset resetFormWidget token ["Passwords do not match"]
        _ -> do
          (resetFormWidget, _) <- generateFormPost resetForm
          renderReset resetFormWidget token []
    Nothing ->
      redirect HomeR
