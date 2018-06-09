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
