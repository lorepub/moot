module Handler.Conference.Roles where

import Import

import Colonnade hiding (fromMaybe)
import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.UUID.V4 (nextRandom)
import Yesod.Colonnade
import qualified Yesod.Paginator as Page

import Handler.Admin
import Handler.Auth
import Helpers.Forms
import qualified Helpers.PaginateUtil as PageUtil
import Helpers.Views

getConferenceRolesR :: ConferenceId -> Handler Html
getConferenceRolesR confId = do
  (_, confEnt@(Entity _ conference)) <- requireAdminForConference confId
  let confName = conferenceName conference
  (admins, editors) <- runDB $ rolesForConference confId
  baseLayout Nothing $ do
    setTitle (fromString (unpack confName))
    [whamlet|
<article .grid-container>
  ^{renderConferenceWidget confEnt}
  <div .medium-6 .cell>
    <h5>
      <a href="@{ConferenceInviteRoleR confId}">
        Invite someone to be an editor on this conference
    <h3>Users with roles on this conference
      <h5>Admins
        <ul>
          $forall (Entity _ user, _) <- admins
            <li>#{tshow user}
      <h5>Editors
        <ul>
          $forall (Entity _ user, _) <- editors
            <li>#{tshow user}
|]

data RoleInviteForm =
  RoleInviteForm {
    role :: Role
  , email :: Email
  } deriving (Eq, Show)

roleInviteForm :: Form RoleInviteForm
roleInviteForm = do
  let roleList :: [(Text, Role)]
      roleList =
        [ ("Editor", EditorRole)
        ]
  renderDivs $
        RoleInviteForm
    <$> areq (selectFieldList roleList)
             (named "role" (placeheld "Role:")) (Just EditorRole)
    <*> areq emailField' (named "email"
                          (placeheld "Email: ")) Nothing

conferenceInviteRoleView :: Entity Conference -> Widget -> Handler Html
conferenceInviteRoleView confEnt@(Entity confId conference) roleInviteWidget = do
  let confName = conferenceName conference
  baseLayout Nothing $ do
    setTitle (fromString (unpack confName))
    [whamlet|
<article .grid-container>
  ^{renderConferenceWidget confEnt}
  <div .medium-6>
    <h3>Invite someone to perform a role for this conference
  <div .medium-6>
    <form method="POST" action="@{ConferenceInviteRoleR confId}">
      ^{roleInviteWidget}
      <input .button type="submit" value="Invite">
|]

inviteEmailToConferenceRole :: ConferenceId
                            -> Email
                            -> Role
                            -> Handler (Entity RoleInvitation)
inviteEmailToConferenceRole confId email role = do
  emailInvitation <- runDB $ createRoleInvitation confId email role
  -- let uuid = editorInvitationUuid emailInvitation
  -- $logInfoSH uuid
  return $ emailInvitation

getConferenceInviteRoleR :: ConferenceId -> Handler Html
getConferenceInviteRoleR confId = do
  (_, confEnt) <- requireAdminForConference confId
  (roleInviteWidget, _) <- generateFormPost roleInviteForm
  conferenceInviteRoleView confEnt roleInviteWidget

postConferenceInviteRoleR :: ConferenceId -> Handler Html
postConferenceInviteRoleR confId = do
  (_, confEnt) <- requireAdminForConference confId
  ((result, roleInviteWidget), _) <- runFormPost roleInviteForm
  case result of
    FormSuccess (RoleInviteForm role email) -> do
      case role of
        EditorRole -> do
          _ <- inviteEmailToConferenceRole confId email role
          (freshRoleInviteWidget, _) <- generateFormPost roleInviteForm
          setMessage $ toHtml $ "You successfully invited: " <> tshow (unEmail email)
          conferenceInviteRoleView confEnt freshRoleInviteWidget
    err -> do
      $logErrorSH err
      conferenceInviteRoleView confEnt roleInviteWidget

getConferenceAcceptRoleR :: ConferenceId -> UUID -> Handler Html
getConferenceAcceptRoleR confId uuid = do
  undefined
