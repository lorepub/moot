module Handler.Admin where

import Import

import Handler.Auth
import Helpers.Forms
import Helpers.Views

getOrganizerSignupR :: Handler Html
getOrganizerSignupR = undefined

postOrganizerSignupR :: Handler Html
postOrganizerSignupR = undefined

getAdminR :: UserId -> Handler Html
getAdminR userId = undefined
  -- User{..} <-
  --   runDBOr404 (get userId)
  -- let isOrIsNot :: Text
  --     isOrIsNot =
  --       if userIsAdmin
  --       then "IS"
  --       else "IS NOT"
  --     header =
  --       [st|#{userEmail} #{isOrIsNot} an Admin!|]
  -- baseLayout Nothing $ do
  --   setTitle "Home"
  --   [whamlet|
  --   <h1>#{header}
  --   <form method="POST" action="@{AdminR userId}">
  --     <input .button type="submit" value="Ascend">
  --   |]

postAdminR :: UserId -> Handler Html
postAdminR userId = undefined
  -- runDB $ update userId [UserIsAdmin =. True]
  -- redirect $ AdminR userId

data AbstractTypeForm =
  AbstractTypeForm {
    abstractTypeFormName :: Text
  , abstractTypeFormDuration :: Word64
  } deriving Show

abstractTypeForm :: Form AbstractTypeForm
abstractTypeForm =
  renderDivs $
        AbstractTypeForm
    <$> areq textField (named "talk-type-name"
                        (placeheld "Talk type name: ")) Nothing
    <*> areq intField (named "talk-duration"
                        (placeheld "Talk type duration in minutes: ")) Nothing

renderConferenceAbstractTypes ::
     Int64
  -> [Entity AbstractType]
  -> Widget
  -> Handler Html
renderConferenceAbstractTypes conferenceId abstractTypes abstractTypeFormWidget = do
  baseLayout Nothing $ do
    setTitle "Conference Abstract Types"
    [whamlet|
<article .grid-container>
  <div .medium-3 .cell>
    <h1>Add a new abstract type
    <div>
      <form method="POST" action=@{ConferenceAbstractTypesR conferenceId}>
        ^{abstractTypeFormWidget}
        <input .button type="submit" value="Create">
  <div .medium-6 .cell>
    <h1>Abstract types
    $if null abstractTypes
      <h5>No talk types are defined yet!
    $else
      <ul>
        $forall abstractType <- abstractTypes
          <li>#{renderAbstractType (entityVal abstractType)}
    |]

getConferenceAbstractTypesR :: Int64 -> Handler Html
getConferenceAbstractTypesR conferenceId = do
  (user, owner, account, conference) <-
    requireOwnerForConference (toSqlKey conferenceId)
  abstractTypes <- runDB $ getAbstractTypes (entityKey conference)
  (abstractTypeFormWidget, _) <- generateFormPost abstractTypeForm
  renderConferenceAbstractTypes conferenceId abstractTypes abstractTypeFormWidget

postConferenceAbstractTypesR :: Int64 -> Handler Html
postConferenceAbstractTypesR conferenceId = do
  (user, owner, account, conference) <-
    requireOwnerForConference (toSqlKey conferenceId)
  ((result, abstractTypeFormWidget), _) <- runFormPost abstractTypeForm
  case result of
    FormSuccess (AbstractTypeForm name duration) -> do
      abstractTypes <- runDB $ do
        void $ insertEntity $ AbstractType (entityKey conference) name (makeTalkDuration duration)
        getAbstractTypes (entityKey conference)
      renderConferenceAbstractTypes conferenceId abstractTypes abstractTypeFormWidget
    _ -> error "bluhhh"

getConferencesR :: Handler Html
getConferencesR = do
  (user, account) <- requireAccount
  conferences <- runDB $ getConferencesByAccount (entityKey account)
  baseLayout Nothing $ do
    setTitle "My Conferences"
    [whamlet|
<article .grid-container>
  <div .medium-6 .cell>
    <div .medium-12 .cell>
      <h1>My Conferences
    <div .medium-12 .cell>
    $if null conferences
      <h5>You haven't created any conferences yet!
    $else
      $forall conf <- conferences
        ^{renderConferenceWidget conf}
|]

--------------------------------------------------------------------------------
-- Conference Dashboard View
--------------------------------------------------------------------------------

getConferenceDashboardR :: Int64 -> Handler Html
getConferenceDashboardR confId = do
  (_, confEntity) <- requireAdminForConference (toSqlKey confId)
  let confName = conferenceName (entityVal confEntity)
  baseLayout Nothing $ do
    setTitle (fromString (unpack confName))
    [whamlet|
<article .grid-container>
  ^{renderConferenceWidget confEntity}
  <div .medium-12 .cell>
    <h2>
      <a href=@{ConferenceCallForProposalsR confId}>
        Call For Proposals
    <h1>
      <a href="@{ConferenceAbstractTypesR confId}">
        Abstract types
|]

renderConferenceWidget :: Entity Conference -> Widget
renderConferenceWidget confEntity =
  [whamlet|
<div .medium-12 .cell>
  <a href=@{ConferenceDashboardR confId}>
    <h1>#{name}
<div .medium-12 .cell>
  <p>#{desc}
|]
  where
    confId = fromSqlKey (entityKey confEntity)
    Conference _ name desc = entityVal confEntity

--------------------------------------------------------------------------------
-- CFP View
--------------------------------------------------------------------------------

getConferenceCallForProposalsR :: Int64 -> Handler Html
getConferenceCallForProposalsR confId = do
  (_, confEntity) <- requireAdminForConference (toSqlKey confId)
  abstracts <- runDB (getAbstractsForConference (toSqlKey confId))
  baseLayout Nothing $ do
    setTitle "Call for Proposals"
    [whamlet|
<article .grid-container>
  <div .medium-12 .cell>
    <h1>#{length abstracts} Abstract Submissions
  $forall abstractAndType <- abstracts
    <div .medium-12 .cell>
      ^{renderAbstractRow abstractAndType}
|]

renderAbstractRow :: (Entity Abstract, Entity AbstractType) -> Widget
renderAbstractRow (abstract, abstractType) =
  [whamlet|
<div .medium-2 .cell>
  <h3>title
<div .medium-4 .cell>
  <h3>#{name}
  <p>#{renderTalkDuration duration}
<div .medium-6 .cell>
  <p>#{contentPreview}
|]
  where
    Abstract user _ title authorAbs meditedAbs = entityVal abstract
    AbstractType _ name duration = entityVal abstractType

    contentPreview = take 100 (fromMaybe authorAbs meditedAbs) <> "..."
