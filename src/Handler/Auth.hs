module Handler.Auth where

import Import

import Handler.Auth.Forms
import Handler.Auth.Views

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
      $logWarn "Current user is not associated with an owner"
      permissionDenied "Current user is not associated with an owner"
    (Just owner) ->
      return (user, owner)

noAccountsForUserMsg :: Text
noAccountsForUserMsg = "No accounts associated with your user"

noConferencesForUserMsg :: Text
noConferencesForUserMsg = "No conferences associated with your user"

requireAccount :: Handler (Entity User, Entity Account)
requireAccount = do
  maybeAccount <- requestAccount
  case maybeAccount of
    (user, Nothing) -> do
      $logWarn "Current user is not associated with an account"
      permissionDenied noAccountsForUserMsg
    (user, Just account) -> return (user, account)

requestAccount :: Handler (Entity User, Maybe (Entity Account))
requestAccount = do
  user <- requireUser
  maybeAccount <- runDB $ getAccountByUser (entityKey user)
  case maybeAccount of
    Nothing -> do
      return (user, Nothing)
    Just account -> return (user, Just account)

requireOwnerForEntity
  :: DBVal a
  => EntityField a (Key Owner)
  -> (SqlExpr (Entity a) -> SqlQuery b)
  -> Handler (Entity User, Entity Owner, Entity a)
requireOwnerForEntity f w = do
  (user, owner) <- requireOwner
  maybeRec <- runDB $ getRecByField' f w (entityKey owner)
  case maybeRec of
    Nothing -> do
      $logWarn "Current user is not Owner for requested resource"
      permissionDenied "You are not an owner for this resource"
    (Just rec') ->
      return (user, owner, rec')

requireOwnerForConference
  :: ConferenceId
  -> Handler ( Entity User, Entity Owner, Entity Account, Entity Conference)
requireOwnerForConference conferenceId = do
  (user, owner) <- requireOwner
  maybeConfAcc <- runDB $ getAccAndConf owner
  case maybeConfAcc of
    Nothing -> do
      $logWarn "Current user is not associated with an account"
      permissionDenied noAccountsForUserMsg
    Just (_, Nothing) -> do
      $logWarn "Current user is not associated with any conferences"
      permissionDenied noConferencesForUserMsg
    Just (acc, Just conf) ->
      return (user, owner, acc, conf)
  where
    getAccAndConf :: Entity Owner -> DB (Maybe (Entity Account, Maybe (Entity Conference)))
    getAccAndConf owner =
      selectFirst $
        from $ \(account `LeftOuterJoin` mConference) -> do
          on (just (account ^. AccountId) ==. mConference ?. ConferenceAccount)
          where_ (account ^. AccountOwner ==. val (entityKey owner))
          return (account, mConference)

requireAdminForConference
  :: ConferenceId
  -> Handler (Entity User, Entity Conference)
requireAdminForConference conferenceId = do
  user <- requireUser
  mConf <- runDB (getOwnerForConference conferenceId)
  case mConf of
    Nothing -> do
      let errMsg = "Conference does not exist"
      $logWarn errMsg >> permissionDenied errMsg
    Just (conf, owner) -> do
      unless (ownerUser (entityVal owner) == entityKey user) $ do
        isAdmin <- runDB (isUserConferenceAdmin (entityKey user))
        unless isAdmin $ do
          let errMsg = "Current User is not an admin of the conference"
          $logWarn errMsg >> permissionDenied errMsg
      pure (user, conf)

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

getResetR :: Handler Html
getResetR = do
  redirectIfLoggedIn HomeR
  maybeToken <- lookupGetParam "token"
  case maybeToken of
    (Just tokenText) -> do
      let token = Token tokenText
      runDB deleteOldResets
      maybeUser <- runDB $ getUserByResetToken token
      case maybeUser of
        (Just _) -> do
          (resetFormWidget, _) <- generateFormPost (resetForm (Just token))
          renderReset resetFormWidget []
        Nothing -> redirect HomeR
    Nothing -> redirect HomeR

postResetR :: Handler Html
postResetR = do
  redirectIfLoggedIn HomeR
  ((result, _), _) <- runFormPost (resetForm Nothing)
  case result of
    FormSuccess resetFormData -> do
      let token = resetTokenVal resetFormData
      runDB deleteOldResets
      maybeUserPassword <- runDB $ getUserPasswordByResetToken token
      case maybeUserPassword of
        (Just _) -> do
          if resetPassword resetFormData == resetConfirm resetFormData
          then do
            runDB $ resetUserPassword token (resetPassword resetFormData)
            (loginFormWidget, _) <- generateFormPost loginForm
            renderLogin loginFormWidget []
          else do
            (resetFormWidget, _) <- generateFormPost (resetForm (Just token))
            renderReset resetFormWidget ["Passwords do not match"]
        Nothing ->
          redirect HomeR
    _ ->
      redirect HomeR
