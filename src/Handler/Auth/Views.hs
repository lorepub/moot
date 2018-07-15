module Handler.Auth.Views where

import Import

import Helpers.Views

renderSignup :: Widget -> [Text] -> Handler Html
renderSignup widget formErrors = do
  baseLayout Nothing $ do
    setTitle "Signup"
    [whamlet|
<article .grid-container>
  <div .grid-x .grid-margin-x>
    <div .medium-6 .cell>
      <h1>Signup for an account!
      $if not (null formErrors)
        <div data-abide-error
             class="alert callout">
            <p>
              <i class="fi-alert"></i>
              $forall errMsg <- formErrors
                <span.error>#{errMsg}
      <div>
        <form method="POST" action="@{SignupR}">
          ^{widget}
          <input .button type="submit" value="Submit">
|]

renderLogin :: Widget -> [Text] -> Handler Html
renderLogin widget formErrors = do
  baseLayout Nothing $ do
    setTitle "Login"
    [whamlet|
<article .grid-container>
  <div .grid-x .grid-margin-x>
    <div .medium-6 .cell>
      <h1>Log into Moot
      $if not (null formErrors)
        <div data-abide-error
             class="alert callout">
            <p>
              <i class="fi-alert"></i>
              $forall errMsg <- formErrors
                <span.error>#{errMsg}
      <div>
        <form action="@{LoginR}" method="POST">
          ^{widget}
          <p>
            <input data-disable-with="Login" name="commit" type="submit" value="Login">
|]
