module Handler.Auth.Forms where

import Import

import Helpers.Forms

loginForm :: Form (Text, Text)
loginForm =
  renderDivs $
  (,) <$> areq textField (named "email" (placeheld "Email")) Nothing
      <*> areq passwordField (named "password" (placeheld "Password")) Nothing
