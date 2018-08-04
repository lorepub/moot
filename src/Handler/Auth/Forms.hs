module Handler.Auth.Forms where

import Import

import Helpers.Forms

loginForm :: Form (Email, Text)
loginForm =
  renderDivs $
  (,) <$> areq emailField' (named "email" (placeheld "Email")) Nothing
      <*> areq passwordField (named "password" (placeheld "Password")) Nothing


data SignupForm = SignupForm {
    signupEmail :: Email
  , signupPassword :: Text
  }

signupForm :: Form SignupForm
signupForm =
  renderDivs $
    SignupForm
      <$> areq emailField' (named "email" (placeheld "Email: ")) Nothing
      <*> areq passwordField (named "password" (placeheld "Password: ")) Nothing

forgotForm :: Form Email
forgotForm =
  renderDivs $
    areq emailField' (named "email" (placeheld "Email")) Nothing

data ResetForm = ResetForm {
    resetPassword :: Text
  , resetConfirm  :: Text
  , resetTokenVal :: Token
  }

resetForm :: Maybe Token -> Form ResetForm
resetForm maybeToken =
  renderDivs $
    ResetForm
      <$> areq passwordField (named "password" (placeheld "Password: ")) Nothing
      <*> areq passwordField (named "confirm" (placeheld "Confirm: ")) Nothing
      <*> areq hiddenField   (named "token" "") maybeToken

