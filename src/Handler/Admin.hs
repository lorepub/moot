module Handler.Admin where

import Import

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

getAbstractTypes :: ConferenceId -> DB [Entity AbstractType]
getAbstractTypes conferenceId =
  getRecsByField AbstractTypeConference conferenceId

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
  baseLayout Nothing $ do
    setTitle "My conferences"
    [whamlet|
|]

getConferenceDashboardR :: Int64 -> Handler Html
getConferenceDashboardR conferenceId = do
  baseLayout Nothing $ do
    setTitle "Dashboard"
    [whamlet|
|]
