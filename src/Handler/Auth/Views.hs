module Handler.Auth.Views where

import Import

import Helpers.Views

renderLogin :: Widget -> Handler Html
renderLogin widget = do
  baseLayout Nothing $ do
    setTitle "Login"
    [whamlet|
<article .grid-container>
  <div .grid-x .grid-margin-x>
    <div .medium-6 .cell>
      <h1>Login
      <div>
      <form action="@{LoginR}" method="POST">
        ^{widget}
        <p>
          <input data-disable-with="Login" name="commit" type="submit" value="Login">
|]
