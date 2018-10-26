module Handler.Auth where

import Import

import Handler.Auth.Forms
import Handler.Auth.Views
import qualified Network.HTTP.Types.Status as H

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
      renderErrorPage H.status401 $ \ maybeCurrentRoute -> [whamlet|
        <h1>Not Authorized
        <h3>You must be logged in to access this page, please login then try again.
        $maybe currentRoute <- maybeCurrentRoute
          <p>You tried to access: <a href="@{currentRoute}">@{currentRoute}</a>
      |]
    (Just user) -> return user

requireVerifiedUser :: Handler (Entity User)
requireVerifiedUser = do
  user <- requireUser
  case userVerifiedAt (entityVal user) of
    Nothing -> do
      $logWarn "User not verified"
      renderErrorPage H.status401 $ \ maybeCurrentRoute -> [whamlet|
        <h1>Not Authorized
        <h3>You must have verified your email address to access this page.
        <p>Please verify your email address and then try again.
        $maybe currentRoute <- maybeCurrentRoute
          <p>You tried to access: <a href="@{currentRoute}">@{currentRoute}</a>
      |]
    (Just _) -> return user

requireOwner :: Handler (Entity User, Entity Account)
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
    (_, Nothing) -> do
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
  => EntityField a (Key Account)
  -> (SqlExpr (Entity a) -> SqlQuery b)
  -> Handler (Entity User, Entity Account, Entity a)
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
  -> Handler (Entity User, Entity Account, Entity Conference)
requireOwnerForConference conferenceId = do
  (user, account) <- requireOwner
  maybeConfAcc <- runDB $ getAccAndConf (accountOwner $ entityVal account)
  case maybeConfAcc of
    Nothing -> do
      $logWarn "Current user is not associated with an account"
      permissionDenied noAccountsForUserMsg
    Just (_, Nothing) -> do
      $logWarn "Current user is not associated with any conferences"
      permissionDenied noConferencesForUserMsg
    Just (acc, Just conf) ->
      return (user, acc, conf)
  where
    getAccAndConf :: UserId -> DB (Maybe (Entity Account, Maybe (Entity Conference)))
    getAccAndConf userId =
      selectFirst $
        from $ \(account `LeftOuterJoin` mConference) -> do
          on (just (account ^. AccountId) ==. mConference ?. ConferenceAccount)
          where_ (account ^. AccountOwner ==. val userId)
          where_ (mConference ?. ConferenceId ==. (just (val conferenceId)))
          return (account, mConference)

-- TODO: requireAdmin
requireAdmin :: Handler (Entity User, Either (Entity Account) (Entity Admin))
requireAdmin = undefined

requireAdminForConference
  :: ConferenceId
  -> Handler (Entity User, Entity Conference)
requireAdminForConference conferenceId = do
  user <- requireVerifiedUser
  mConf <- runDB (getOwnerForConference conferenceId)
  case mConf of
    Nothing -> do
      let errMsg = "Conference does not exist"
      $logWarn errMsg >> permissionDenied errMsg
    Just (conf, _, ownerUser) -> do
      unless (entityKey ownerUser == entityKey user) $ do
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

getVerifyR :: UUID -> Handler Html
getVerifyR token = do
  runDBOr404 $ verifyEmail token
  renderVerify

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
            (Entity dbUserKey _, Entity _ emailVerification) <-
              createUser signupEmail signupName signupPassword
            $logInfoSH (emailVerificationUuid emailVerification)
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
