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
  , abstractTypeFormDuration :: Text
  } deriving Show

abstractTypeForm :: Form AbstractTypeForm
abstractTypeForm =
  renderDivs $
        AbstractTypeForm
    <$> areq textField (named "talk-type-name"
                        (placeheld "Talk type name: ")) Nothing
    <*> areq textField (named "talk-duration"
                        (placeheld "Talk type duration: ")) Nothing

getConferenceAbstractTypesR :: Int64 -> Handler Html
getConferenceAbstractTypesR conferenceId = do
  (user, owner, conference) <-
    requireOwnerForConference conferenceId
  abstractTypes <-
    runDB $
      getRecsByField
        AbstractTypeConference
        (toSqlKey conferenceId)
  baseLayout Nothing $ do
    setTitle "Conference Abstract Types"
    [whamlet|
    <h1>Abstract types
    <ul>
      $forall abstractType <- abstractTypes
        <li>#{tshow abstractType}
    |]

postConferenceAbstractTypesR :: Int64 -> Handler Html
postConferenceAbstractTypesR conferenceId = do
  (user, owner, conference) <-
    requireOwnerForConference conferenceId
  return [whamlet||]
