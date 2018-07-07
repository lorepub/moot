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
    Nothing -> redirect HomeR -- LoginR
    (Just user) -> return user

requireOwner :: Handler (Entity User, Entity Owner)
requireOwner = do
  user <- requireUser
  maybeOwner <- runDB $ getOwnerForUser (entityKey user)
  case maybeOwner of
    Nothing -> permissionDenied "You are not an owner"
    (Just owner) ->
      return (user, owner)

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
    Nothing -> permissionDenied "You are not an owner for this resource"
    (Just rec') ->
      return (user, owner, rec')

requireOwnerForConference conferenceId =
    requireOwnerForEntity
       ConferenceOwner
       (\conf -> where_ (conf ^. ConferenceId ==. val (toSqlKey conferenceId)))

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

renderSignup :: Widget -> Handler Html
renderSignup widget = do
  baseLayout Nothing $ do
    setTitle "Signup"
    [whamlet|
<div>
  <div>
    <hr>
<div>
  <div>
    <h3>Signup for an account!
    <form method="POST" action="@{SignupR}">
      ^{widget}
      <input .button type="submit" value="Submit">
|]

getSignupR :: Handler Html
getSignupR = do
  redirectIfLoggedIn HomeR
  (signupFormWidget, _) <- generateFormPost signupForm
  renderSignup signupFormWidget

postSignupR :: Handler Html
postSignupR = do
  redirectIfLoggedIn HomeR
  ((result, widget), _) <- runFormPost signupForm
  case result of
    FormSuccess SignupForm{..} -> do
      runDB $ do
        maybeUP <- getUserByEmail signupEmail
        case maybeUP of
          (Just _) ->
            lift $ renderSignup widget
          Nothing -> do
            (Entity dbUserKey _) <-
              createUser signupEmail signupPassword
            lift $ setUserSession dbUserKey True
            lift $ redirect HomeR
      -- Check to see if a user with this email already exists
      -- maybeUP <- runDB $ getUserByEmail signupEmail
      -- case maybeUP of
      --   -- If it does, render the form again (?)
      --   (Just _) -> do
      --     renderSignup widget
      --   -- If not, create a user
      --   Nothing -> do
      --     (Entity dbUserKey _) <-
      --       runDB $ createUser signupEmail signupPassword
      --     setUserSession dbUserKey True
      --     redirect HomeR
    _ -> renderSignup widget

getSignoutR :: Handler Html
getSignoutR = do
  deleteLoginData
  redirect HomeR
