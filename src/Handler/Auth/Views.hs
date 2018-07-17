module Handler.Auth.Views where

import Import

import Helpers.Views

mediumContainer :: Widget -> Widget
mediumContainer widget = [whamlet|
<article .grid-container>
  <div .grid-x .grid-margin-x>
    <div .medium-6 .cell>
      ^{widget}
|]

formErrorWidget :: [Text] -> Widget
formErrorWidget formErrors = [whamlet|
  $if not (null formErrors)
    <div data-abide-error
         class="alert callout">
      <p>
        <i class="fi-alert"></i>
        $forall errMsg <- formErrors
          <span.error>#{errMsg}
|]

renderSignup :: Widget -> [Text] -> Handler Html
renderSignup widget formErrors = do
  baseLayout Nothing $ do
    setTitle "Signup"
    mediumContainer $ [whamlet|
      <h1>Signup for an account!
      ^{formErrorWidget formErrors}
      <div>
        <form method="POST" action="@{SignupR}">
          ^{widget}
          <input .button type="submit" value="Submit">
|]

renderLogin :: Widget -> [Text] -> Handler Html
renderLogin widget formErrors = do
  baseLayout Nothing $ do
    setTitle "Login"
    mediumContainer $ [whamlet|
      <h1>Log into Moot
      ^{formErrorWidget formErrors}
      <div>
        <form action="@{LoginR}" method="POST">
          ^{widget}
          <div .text-right>
            <a href="@{ForgotR}">Forgot Password
          <p>
            <input .button data-disable-with="Login" name="commit" type="submit" value="Login">
|]

renderForgot :: Widget -> [Text] -> Handler Html
renderForgot widget formErrors = do
  baseLayout Nothing $ do
    setTitle "Forgot Password"
    mediumContainer $ [whamlet|
      <h1>Forgot Password
      ^{formErrorWidget formErrors}
      <div>
        <form method="POST" action="@{ForgotR}">
          ^{widget}
          <input .button type="submit" value="Submit">
|]

renderReset :: Widget -> Token -> [Text] -> Handler Html
renderReset widget token formErrors = do
  baseLayout Nothing $ do
    setTitle "Reset Password"
    mediumContainer $ [whamlet|
      <h1>Reset Password
      ^{formErrorWidget formErrors}
      <div>
        <form method="POST" action="@{ResetR token}">
          ^{widget}
          <input .button type="submit" value="Submit">
|]

renderNotice :: Text -> [Text] -> Handler Html
renderNotice header messages = do
  baseLayout Nothing $ do
    mediumContainer $ [whamlet|
      <h1>#{header}
      $if not (null messages)
        <div data-abide-error
             class="success callout">
            <p>
              <i class="fi-alert"></i>
              $forall msg <- messages
                <span>#{msg}
|]

