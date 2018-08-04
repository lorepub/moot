module Handler.Admin where

import Import

import Colonnade hiding (fromMaybe)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Yesod.Colonnade

import Handler.Auth
import Helpers.Forms
import Helpers.Handlers
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
     ConferenceId
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

getConferenceAbstractTypesR :: ConferenceId -> Handler Html
getConferenceAbstractTypesR conferenceId = do
  (user, owner, account, conference) <-
    requireOwnerForConference conferenceId
  abstractTypes <- runDB $ getAbstractTypes (entityKey conference)
  (abstractTypeFormWidget, _) <- generateFormPost abstractTypeForm
  renderConferenceAbstractTypes conferenceId abstractTypes abstractTypeFormWidget

postConferenceAbstractTypesR :: ConferenceId -> Handler Html
postConferenceAbstractTypesR conferenceId = do
  (user, owner, account, conference) <-
    requireOwnerForConference conferenceId
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

getConferenceDashboardR :: ConferenceId -> Handler Html
getConferenceDashboardR confId = do
  (_, confEnt@(Entity _ conference)) <- requireAdminForConference confId
  let confName = conferenceName conference
  baseLayout Nothing $ do
    setTitle (fromString (unpack confName))
    [whamlet|
<article .grid-container>
  ^{renderConferenceWidget confEnt}
  <div .medium-12 .cell>
    <h2>
      <a href=@{ConferenceCallForProposalsR confId}>
        Call For Proposals
    <h2>
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
    confId = entityKey confEntity
    Conference _ name desc = entityVal confEntity


getConferenceCallForProposalsR :: ConferenceId -> Handler Html
getConferenceCallForProposalsR confId = do
  (_, confEntity) <- requireAdminForConference confId
  abstracts <- runDB (getAbstractsForConference confId)
  let ct = encodeCellTable [] (colonnadeAbstracts confId) abstracts

  baseLayout Nothing $ do
    setTitle "Call for Proposals"
    [whamlet|
<article .grid-container>
  <div .row>
    <div .medium-9 .column>
      <h1>#{length abstracts} Abstract Submissions
  <div .row>
    <div .medium-9 .column>
      ^{ct}
|]

colonnadeAbstracts :: ConferenceId
                   -> Colonnade
                      Headed
                      (Entity Abstract, Entity AbstractType)
                      (Cell App)
colonnadeAbstracts confId =
  mconcat [
    headed "Title" (cell . titleF . fst)
  , headed "Name" (textCell . abstractNameF . snd)
  , headed "Content" (textCell . contentF . entityVal . fst)
  ]
  where titleF (Entity abstractK abstract) =
          [whamlet|
          <a href=@{ConferenceAbstractR confId abstractK}>
            #{abstractTitle abstract}
          |]
        abstractNameF (Entity _ abstractType) =
          abstractTypeName abstractType
        contentF abstract =
          (take 100
            (fromMaybe
             (abstractAuthorAbstract abstract)
             (abstractEditedAbstract abstract)
            ))

data EditedAbstract =
  EditedAbstract {
    newAbstractTitle :: Text
  , newAbstractBody :: Textarea
  } deriving (Eq, Show)

abstractEditForm :: Maybe Text -> Maybe Textarea -> Form EditedAbstract
abstractEditForm editedTitle editedBody = do
  renderDivs $
      EditedAbstract
      <$> areq textField (named "abstract-title"
                          (placeheld "Abstract title:")) editedTitle
      <*> areq textareaField (named "abstract-body"
                              (placeheld "Abstract proposal:")) editedBody

getConferenceAbstractR :: ConferenceId -> AbstractId -> Handler Html
getConferenceAbstractR confId abstractId = do
  (_, Entity _ conference) <- requireAdminForConference confId
  Abstract{..} <- runDBOr404 $ get abstractId
  (widget, enctype) <-
    generateFormPost
      (abstractEditForm (Just abstractTitle) (Textarea <$> abstractEditedAbstract))
  baseLayout Nothing $ do
    setTitle "Abstract"
    [whamlet|
<article .grid-container>
  <div .row>
    <div .medium-9 .column>
      <h1>Editing abstract #{tshow (fromSqlKey abstractId)}
      <h3>Conference: #{conferenceName conference}
      <label>Title: #{abstractTitle}
      <label>Speaker-submitted abstract:
        <pre>#{abstractAuthorAbstract}
      <form method=POST enctype=#{enctype} action=/>
        ^{widget}
        <input .button type="submit" value="Submit abstract">
|]
