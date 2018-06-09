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

getLoginR :: Handler Html
getLoginR = do
  redirectIfLoggedIn HomeR
  (loginFormWidget, _) <- generateFormPost loginForm
  renderLogin loginFormWidget

postLoginR :: Handler Html
postLoginR = do
  redirectIfLoggedIn HomeR
  ((result, widget), _) <- runFormPost loginForm
  case result of
    FormSuccess (email, password) -> do
      maybeUP <- runDB (getUserPassword email)
      case maybeUP of
        Nothing ->
          notFound
        (Just
         ( Entity dbUserKey _
         , Entity _ Password{..})) -> do
          let success =
                passwordMatches passwordHash password
          case success of
            False ->
              notAuthenticated
            True -> do
              setUserSession dbUserKey True
              redirect HomeR
    _ -> renderLogin widget


data SignupForm = SignupForm {
    signupEmail :: Text
  , signupUsername :: Text
  , signupPassword :: Text
  }

signupForm :: Form SignupForm
signupForm =
  renderDivs $
    SignupForm
      <$> areq textField (named "email" (placeheld "Email: ")) Nothing
      <*> areq textField (named "username" (placeheld "Username: ")) Nothing
      <*> areq passwordField (named "password" (placeheld "Password: ")) Nothing

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
      -- Check to see if a user with this email already exists
      maybeUP <- runDB (getUserByEmail signupEmail)
      case maybeUP of
        -- If it does, render the form again (?)
        (Just _) -> do
          renderSignup widget
        -- If not, create a user
        Nothing -> do
          (Entity dbUserKey _) <-
            runDB $ createUser signupEmail signupPassword
          setUserSession dbUserKey True
          redirect HomeR
    _ -> renderSignup widget

getSignoutR :: Handler Html
getSignoutR = do
  deleteLoginData
  redirect HomeR
