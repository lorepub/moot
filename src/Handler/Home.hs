module Handler.Home where

import Import

import Helpers.Views

getHomeR :: Handler Html
getHomeR = do
  baseLayout Nothing $ do
    setTitle "Home"
    [whamlet|
<article .grid-container>
  <div .grid-x .cell>
    <nav aria-label="You are here:" role="navigation">
      <ul .breadcrumbs>
        <li>
          <a href="#">
            Home
        <li>
          <a href="#">
            Features
        <li .disabled>
          Gene Splicing
        <li>
          <span .show-for-sr>
            Current: 
          Cloning
  <div .grid-x .grid-margin-x>
    <div .medium-6 .cell>
      <img .thumbnail src="https://placehold.it/650x350">
      <div .grid-x .grid-padding-x .small-up-4>
        <div .cell>
          <img src="https://placehold.it/250x200">
        <div .cell>
          <img src="https://placehold.it/250x200">
        <div .cell>
          <img src="https://placehold.it/250x200">
        <div .cell>
          <img src="https://placehold.it/250x200">
    <div .medium-6 .large-5 .cell .large-offset-1>
      <h3>
        My Awesome Product
      <p>
        Nunc eu ullamcorper orci. Quisque eget odio ac lectus vestibulum faucibus eget in metus. In pellentesque faucibus vestibulum. Nulla at nulla justo, eget luctus tortor. Nulla facilisi. Duis aliquet egestas purus in.
      <label>
        Size
        <select>
          <option value="husker">
            Small
          <option value="starbuck">
            Medium
          <option value="hotdog">
            Large
          <option value="apollo">
            Yeti
      <div .grid-x>
        <div .small-3 .cell>
          <label .middle for="middle-label">
            Quantity
        <div .small-9 .cell>
          <input #middle-label placeholder="One fish two fish" type="text">
      <a .button .large .expanded href="#">
        Buy Now
      <div .small .secondary .expanded .button-group>
        <a .button>
          Facebook
        <a .button>
          Twitter
        <a .button>
          Yo
|]

getContactR :: Handler Html
getContactR = do
  baseLayout Nothing $ do
    setTitle "Contact"
    [whamlet|
<article .grid-container>
  <div .grid-x .grid-margin-x>
    <div .medium-6 .large-5 .cell .large-offset-1>
      <h2>
        Contact 
      <p>
        LorePub HQ Address
      <p>
        <b> For sales, 
        send us an email at 
        <a href="mailto:#"> customers@lorepub.com
      <p>
        <b> For support, 
        <a href="#"> submit a ticket
        or 
        <a href="https://twitter.com/bitemyapp"> tweet directly at Chris Allen.
|]
