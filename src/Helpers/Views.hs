module Helpers.Views where

import Import

baseLayout :: Maybe (Entity User)
           -> WidgetFor App ()
           -> HandlerFor App Html
baseLayout _ content =
  defaultLayout $ do
    addScriptRemote "https://code.jquery.com/jquery-3.3.1.slim.min.js"
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/foundation/6.4.3/js/foundation.min.js"
    addScript (StaticR js_app_js)
    addStylesheet (StaticR css_app_css)
    [whamlet|
^{renderNav}
<br>
^{content}
^{renderFooter}
|]

renderNav :: Widget
renderNav = [whamlet|
<div .top-bar>
  <div .top-bar-left>
    <ul .dropdown .menu data-dropdown-menu="" data-e="ja9juo-e" role="menubar">
      <li role="menuitem">
        <a href="@{HomeR}">
          Moot

      <li aria-haspopup="true" aria-label="One" .has-submenu .is-dropdown-submenu-parent .opens-right data-is-click="false" role="menuitem">
        <a href="#">
          One
        <ul .submenu .menu .vertical .is-dropdown-submenu .first-sub data-submenu="" role="menu" style="">
          <li .is-submenu-item .is-dropdown-submenu-item role="menuitem">
            <a href="#">
              One
          <li .is-submenu-item .is-dropdown-submenu-item role="menuitem">
            <a href="#">
              Two
          <li .is-submenu-item .is-dropdown-submenu-item role="menuitem">
            <a href="#">
              Three
      <li role="menuitem">
        <a href="#">
          Two
  <div .top-bar-right>
    <ul .menu>
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
        This application is free software, &nbsp;
        <a style="text-decoration: underline;">know your rights!
|]
