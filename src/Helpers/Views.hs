module Helpers.Views where

import Import

baseLayout :: Maybe (Entity User)
           -> WidgetFor App ()
           -> HandlerFor App Html
baseLayout _ content = do
  nav <- renderNav
  defaultLayout $ do
    -- We need AJAX here and there.
    addScriptRemote "https://code.jquery.com/jquery-3.3.1.min.js"
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/foundation/6.4.3/js/foundation.min.js"
    addScript (StaticR js_app_js)
    addStylesheet (StaticR css_app_css)
    [whamlet|
^{nav}
<br>
^{content}
^{renderFooter}
|]

renderNav :: Handler Widget
renderNav = do
  maybeUser <- getUser
  rightNav <- renderRightNav maybeUser
  return [whamlet|
<div .top-bar>
  <div .top-bar-left>
    <ul .dropdown .menu data-dropdown-menu="" data-e="ja9juo-e" role="menubar">
      <li role="menuitem">
        <a href="@{HomeR}">
          Moot
      $maybe _ <- maybeUser      
        <li role="menuitem">
          <a href="@{ConferencesR}">
            My Conferences
  ^{rightNav}
|]

renderRightNav :: Maybe (Entity User) -> Handler Widget
renderRightNav maybeUser = do
  return [whamlet|
  <div .top-bar-right>
    <ul .menu>
      $maybe _ <- maybeUser
        <li role="menuitem">
          <a href="@{SignoutR}">
            Signout
      $nothing
        <li role="menuitem">
          <a href="@{SignupR}">
            Signup
        <li role="menuitem">
          <a href="@{LoginR}">
            Login
|]

renderFooter :: Widget
renderFooter = [whamlet|
<hr>
<footer .grid-container>
  <div .grid-x .align-justify .align-middle>
    <div .small-6 .medium-shrink .cell>
      <ul .menu>
        <li .align-self-middle>
          Moot
        <li>
          <a href="@{HomeR}">
            Home
        <li>
          <a href="/about">
            About
        <li>
          <a href="/contact">
            Contact

    <div .small-6 .medium-shrink .cell>
      <span>
        Copyright 2018
      <p>
        Made with ♥ in Austin, TX and around the world
      <div>
        This application is free software, 
        <a style="text-decoration: underline;">know your rights!
|]
