module Handler.Contact where

import Import

import Helpers.Views

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
