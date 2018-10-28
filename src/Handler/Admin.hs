module Handler.Admin where

import Import

import Colonnade hiding (fromMaybe)
import qualified Data.Aeson as A
import qualified Data.Map as M
import Yesod.Colonnade
import qualified Yesod.Paginator as Page
import qualified Helpers.PaginateUtil as PageUtil

import Handler.Auth
import Helpers.Forms
import Helpers.Views

getOrganizerSignupR :: Handler Html
getOrganizerSignupR = undefined

postOrganizerSignupR :: Handler Html
postOrganizerSignupR = undefined

-- getAdminR :: UserId -> Handler Html
-- getAdminR userId = undefined
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

-- postAdminR :: UserId -> Handler Html
-- postAdminR userId = undefined
  -- runDB $ update userId [UserIsAdmin =. True]
  -- redirect $ AdminR userId

data AbstractTypeForm =
  AbstractTypeForm {
    abstractTypeFormName :: Text
  , abstractTypeFormDuration :: Word64
  } deriving Show

mkAbstractTypeForm' :: AbstractType -> Form AbstractTypeForm
mkAbstractTypeForm' at = mkAbstractTypeForm (Just $ abstractTypeName at) (Just $ (unpackTalkDuration . abstractTypeDuration) at)

mkAbstractTypeForm :: Maybe Text -> Maybe Word64 -> Form AbstractTypeForm
mkAbstractTypeForm atName atDuration =
  renderDivs $
        AbstractTypeForm
    <$> areq textField (named "talk-type-name"
                        (placeheld "Talk type name: "))
        atName
    <*> areq intField (named "talk-duration"
                        (placeheld "Talk type duration in minutes: "))
        atDuration

abstractTypeForm = mkAbstractTypeForm Nothing Nothing

renderConferenceAbstractTypes ::
     Entity Conference
  -> [Entity AbstractType]
  -> Widget
  -> Handler Html
renderConferenceAbstractTypes conf@(Entity conferenceId _)
  abstractTypes abstractTypeFormWidget = do
  baseLayout Nothing $ do
    setTitle "Conference Abstract Types"
    [whamlet|
<article .grid-container>
  <div .medium-3 .cell>
    ^{renderConferenceWidget conf}
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
          <li>^{renderAbstractTypeEdit conferenceId abstractType}
    |]

renderConferenceAbstractType ::
     Entity Conference
  -> Entity AbstractType
  -> Widget
  -> Handler Html
renderConferenceAbstractType conf@(Entity conferenceId _)
  abstractType@(Entity abstractTypeId _) abstractTypeFormWidget = do
  baseLayout Nothing $ do
    setTitle "Conference Abstract Type"
    [whamlet|
<article .grid-container>
  <div .medium-3 .cell>
    ^{renderConferenceWidget conf}
  <div .medium-3 .cell>
    <h1> Edit abstract type
    <div>
      <form method="POST" action=@{ConferenceAbstractTypeR conferenceId abstractTypeId }>
        ^{abstractTypeFormWidget}
        <input .button type="submit" value="Create">
    |]

getConferenceAbstractTypesR :: ConferenceId -> Handler Html
getConferenceAbstractTypesR conferenceId = do
  (_, _, _, conference) <-
    requireOwnerForConference conferenceId
  abstractTypes <- runDB $ getAbstractTypes (entityKey conference)
  (abstractTypeFormWidget, _) <- generateFormPost abstractTypeForm
  renderConferenceAbstractTypes conference abstractTypes abstractTypeFormWidget

postConferenceAbstractTypesR :: ConferenceId -> Handler Html
postConferenceAbstractTypesR conferenceId = do
  (_, _, _, conference) <-
    requireOwnerForConference conferenceId
  ((result, abstractTypeFormWidget), _) <- runFormPost abstractTypeForm
  case result of
    FormSuccess (AbstractTypeForm name duration) -> do
      abstractTypes <- runDB $ do
        void $
          insertEntity $
          AbstractType (entityKey conference) name (makeTalkDuration duration)
        getAbstractTypes (entityKey conference)
      renderConferenceAbstractTypes conference abstractTypes abstractTypeFormWidget
    _ -> error "bluhhh"

getConferenceAbstractTypeR :: ConferenceId -> AbstractTypeId -> Handler Html
getConferenceAbstractTypeR conferenceId abstractTypeId = do
  (_, _, _, conference) <-
    requireOwnerForConference conferenceId
  abstractType <- runDBOr404 $ getAbstractTypeByConferenceAndId conferenceId abstractTypeId
  (abstractTypeFormWidget, _) <- generateFormPost (mkAbstractTypeForm' (entityVal abstractType))
  renderConferenceAbstractType conference abstractType abstractTypeFormWidget

postConferenceAbstractTypeR :: ConferenceId -> AbstractTypeId -> Handler Html
postConferenceAbstractTypeR confId abstractTypeId = do
  (_, Entity _ conference) <-
    requireAdminForConference confId
  abstractType <-
    runDBOr404 $ get abstractTypeId
  ((result, widget), enctype) <-
    runFormPost
      (mkAbstractTypeForm' abstractType)
  case result of
    FormSuccess (AbstractTypeForm name duration) -> do
      runDB $ updateAbstractType abstractTypeId name duration
      abstractTypes <- runDB $ getAbstractTypes confId
      (abstractTypeFormWidget, _) <- generateFormPost abstractTypeForm
      renderConferenceAbstractTypes
        (Entity confId conference)
        abstractTypes abstractTypeFormWidget
    _ ->
      renderConferenceAbstractType
        (Entity confId conference)
        (Entity abstractTypeId abstractType) widget

renderAbstractTypeEdit :: ConferenceId -> Entity AbstractType -> Widget
renderAbstractTypeEdit  confId (Entity atId (AbstractType _ name td)) =
  [whamlet|
    <label>
      #{name} (#{renderTalkDuration td})
    <a href=@{ConferenceAbstractTypeR confId atId}>
      [Edit]
  |]

renderConferencesCallout :: [Entity Conference] -> Text -> Widget
renderConferencesCallout [] _ = return ()
renderConferencesCallout xs label =
  [whamlet|
<div .callout>
  <h5>#{label}
  $forall conf <- xs
    ^{renderConferenceWidget conf}
|]

getConferencesR :: Handler Html
getConferencesR = do
  (user, account) <- requestAccount
  let maybeAccountConfsW = fmap renderAccountConferences account
  baseLayout Nothing $ do
    setTitle "My Conferences"
    [whamlet|
<article .grid-container>
  ^{renderSubmitterConferences (entityKey user)}
  <hr>
  $maybe widget <- maybeAccountConfsW
    ^{widget}
  $nothing
    <h5>You aren't managing any conferences
    <p>Would you like to <a>create a conference?</a>
|]

groupTriplets :: [( Entity Conference
                  , Entity AbstractType
                  , Entity Abstract
                  )]
              -> Map ConferenceId
                 ( Entity Conference
                 , [( Entity AbstractType
                    , Entity Abstract
                    )])
groupTriplets ts =
  foldl' f M.empty ts
  where f m (conference, abstractType, abstract) =
          M.insertWith
            g
            (entityKey conference)
            (conference, [(abstractType, abstract)])
            m
        g (k, xs) (_, ys) = (k, xs <> ys)

renderSubmitterConferences :: UserId
                           -> Widget
renderSubmitterConferences userId = do
  triplets <- handlerToWidget $ runDB $ getConferencesBySubmissions userId
  case triplets of
    [] -> [whamlet|
  <h5>You haven't submitted to any conferences
|]
    xs -> do
      let grouped = groupTriplets xs
      [whamlet|
  <div .small-2 .cell>
    <h1>Conferences I have submitted to
  <div .small-4 .cell>
    $forall (conference, abstractPairs) <- grouped
      ^{renderConferenceSubmission conference abstractPairs}
|]

renderConferenceSubmission :: Entity Conference
                           -- -> Entity AbstractType
                           -- -> Entity Abstract
                           -> [( Entity AbstractType
                               , Entity Abstract
                               )]
                           -> Widget
renderConferenceSubmission (Entity confId Conference{..}) abstractPairs = do
  let abstractStatus :: Abstract -> Text
      abstractStatus abstr =
        if abstractIsDraft abstr
        then "Draft"
        else "Submitted"
      draftLink :: ConferenceId -> Entity Abstract -> Maybe (Route App)
      draftLink confId' (Entity abstractId abstract) =
        if abstractIsDraft abstract
        then Just $ AbstractDraftR confId' abstractId
        else Nothing
  [whamlet|
    <h5>My abstracts submitted to #{conferenceName}
    <ul>
      $forall (Entity _ abstractType, abs@(Entity _ abstract)) <- abstractPairs
        $maybe link <- draftLink confId abs
          <a href=@{link}><li>#{abstractStatus abstract}: #{abstractAuthorTitle abstract} - #{renderAbstractType abstractType}
        $nothing
          <li>#{abstractStatus abstract}: #{abstractAuthorTitle abstract} - #{renderAbstractType abstractType}
|]

renderAccountConferences :: Entity Account
                         -> Widget
renderAccountConferences account = do
  conferences <- handlerToWidget $ runDB $ getConferencesByAccount (entityKey account)
  t <- liftIO getCurrentTime
  let ArrangedConferences{..} = arrangeConferencesByStatus t conferences
  [whamlet|
  <div .small-2 .cell>
    <h1>Conferences I am managing
  <div .small-4 .cell>
    $if null conferences
      <h5>You haven't created any conferences yet!
    $else
      ^{renderConferencesCallout notYetOpenConfs "Conferences yet to be opened for CFP submission"}
      ^{renderConferencesCallout openConfs "Conferences open for submissions"}
      ^{renderConferencesCallout closedConfs "Conferences closed to CFP submissions"}
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
    <h5>
      <a href=@{ConferenceCallForProposalsR confId}>
        Call For Proposals
    <h5>
      <a href="@{ConferenceAbstractTypesR confId}">
        Abstract types
    <h5>
      <a href="@{ConferenceSurrogateAbstractR confId}">
        Submit surrogate abstract on behalf of a speaker
  <div .medium-6>
    <div. .medium-3 .column>
      <form method=POST
            action=@{ConferenceCfpOpenR confId}
            #focus>
        <label>
          This will open the conference's CFP, permitting
          third parties to submit abstracts to this
          conference. It will also wipe any previously set
          closing date.
        <input .button .success type="submit" value="Open CFP">
    <div. .medium-3 .column>
      <form method=POST
            action=@{ConferenceCfpCloseR confId}
            #focus>
        <label>
          This will close the conference's CFP for submissions,
          preventing further abstract submissions. If an exception
          needs to be made after closing, use the
          <a href="/admin/abstract/submit">
            admin abstract submission process.
        <input .button .warning type="submit" value="Close CFP">
|]

data CfpStatus =
    CfpNotOpened UTCTime
  | CfpOpen UTCTime
  | CfpOpenUntil UTCTime UTCTime
  | CfpClosed UTCTime UTCTime
  deriving Show

conferenceStatus :: UTCTime -> Conference -> CfpStatus
conferenceStatus currentTime Conference{..} =
  case (conferenceOpened, conferenceClosed) of
       (Nothing, Nothing) -> CfpNotOpened currentTime
       (Just _, Nothing) -> CfpOpen currentTime
       (_, Just closingTime) ->
         case compare currentTime closingTime of
           LT -> CfpOpenUntil currentTime closingTime
           EQ -> CfpOpenUntil currentTime closingTime
           GT -> CfpClosed currentTime closingTime

renderConferenceStatus :: UTCTime -> Conference -> Text
renderConferenceStatus currentTime conference =
  let notOpenedMsg = "Not yet opened"
      openMsg = "Open for submissions"
      openUntilMsg :: Day -> Text
      openUntilMsg t = "Open for submissions until " <> tshow t
      closedSinceMsg :: Day -> Text
      closedSinceMsg t = "Closed since " <> tshow t
  in case conferenceStatus currentTime conference of
       CfpNotOpened _ -> notOpenedMsg
       CfpOpen _ -> openMsg
       CfpOpenUntil _ closeTime -> openUntilMsg (utctDay closeTime)
       CfpClosed _ closeTime -> closedSinceMsg (utctDay closeTime)

data ArrangedConferences =
  ArrangedConferences {
    notYetOpenConfs :: [Entity Conference]
  , openConfs :: [Entity Conference]
  , closedConfs :: [Entity Conference]
  } deriving Show

arrangeConferencesByStatus :: UTCTime -> [Entity Conference] -> ArrangedConferences
arrangeConferencesByStatus currentTime confs =
  foldl' sortingHat (ArrangedConferences [] [] []) confs
  where sortingHat (ArrangedConferences nyo o c) e@(Entity _ conf) =
          case conferenceStatus currentTime conf of
            CfpNotOpened _ ->
              ArrangedConferences (e : nyo) o c
            CfpOpen _ ->
              ArrangedConferences nyo (e : o) c
            CfpOpenUntil _ _ ->
              ArrangedConferences nyo (e : o) c
            CfpClosed _ _ ->
              ArrangedConferences nyo o (e : c)

renderConferenceWidget :: Entity Conference -> Widget
renderConferenceWidget (Entity confId conf@Conference{..}) = do
  t <- liftIO getCurrentTime
  [whamlet|
<div .grid-x>
  <div .medium-3 .cell>
    <a href=@{ConferenceDashboardR confId}>
      <h3>#{conferenceName}
  <div .small-2 .cell .center-flex>
    <label>
      #{renderConferenceStatus t conf}
<div .grid-x>
  <div .medium-12 .cell>
    <p>#{conferenceDescription}
|]

postConferenceCfpOpenR :: ConferenceId -> Handler Html
postConferenceCfpOpenR confId = do
  _ <- requireOwnerForConference confId
  runDB $ openConferenceCfp confId
  redirect $ ConferenceDashboardR confId

postConferenceCfpCloseR :: ConferenceId -> Handler Html
postConferenceCfpCloseR confId = do
  _ <- requireOwnerForConference confId
  runDB $ closeConferenceCfp confId
  redirect $ ConferenceDashboardR confId

data CfpFilterForm =
  CfpFilterForm {
    filterAbstractTitle :: Maybe Text
    -- This is intentionally not `Maybe Email`.
  , filterAuthorEmail :: Maybe Text
  , filterAuthorName :: Maybe Text
  , filterAbstractStatus :: Maybe Bool
  , filterAbstractType :: Maybe AbstractTypeId
  } deriving Show

cfpFilterForm :: [Entity AbstractType] -> Form CfpFilterForm
cfpFilterForm abstractTypes = do
  let abstractTypeList :: [(Text, AbstractTypeId)]
      abstractTypeList =
        map
        renderAbstractTypeDropdown
        abstractTypes
      abstractStatusList :: [(Text, Bool)]
      abstractStatusList =
        [ ("Draft", True)
        , ("Submitted", False)
        ]
  renderDivs $
        CfpFilterForm
    <$> aopt textField (named "abstract-title"
                        (placeheld "CFP Title: ")) Nothing
    -- We are intentionally not validating this as an email address
    -- so that users can search for emails by fragments.
    <*> aopt textField (named "author-email"
                          (placeheld "Author Email: ")) Nothing
    <*> aopt textField (named "author-name"
                        (placeheld "Author Name: ")) Nothing
    <*> aopt (selectFieldList abstractStatusList)
             (named "abstract-status" (placeheld "Abstract status:")) Nothing
    <*> aopt (selectFieldList abstractTypeList)
             (named "abstract-type" (placeheld "Abstract type:")) Nothing

dummyCfpFilterForm :: CfpFilterForm
dummyCfpFilterForm = CfpFilterForm Nothing Nothing Nothing Nothing Nothing

ilikeVal :: ( SqlString s
            , PersistEntity val
            , Esqueleto query expr backend
            )
         => expr (Entity val)
         -> EntityField val s
         -> s
         -> expr (Value Bool)
ilikeVal ref col v =
  (ref ^. col `ilike` (%) ++. val v ++. (%))

genFilterConstraints :: (Esqueleto query expr backend)
                     => CfpFilterForm
                     -> expr (Entity AbstractType)
                     -> expr (Entity Abstract)
                     -> expr (Entity User)
                     -> query ()
genFilterConstraints CfpFilterForm{..} abstractType abstract user = do
  case filterAbstractTitle of
    Nothing -> return ()
    (Just "") -> return ()
    (Just title) -> where_ $ ilikeVal abstract AbstractAuthorTitle title
  case filterAuthorEmail of
    Nothing -> return ()
    (Just email) ->
      where_
        $ ilikeVal user UserEmail (Email email)
  case filterAuthorName of
    Nothing -> return ()
    (Just "") -> return ()
    (Just name) -> where_ $ ilikeVal user UserName name
  case filterAbstractStatus of
    Nothing -> return ()
    (Just v) -> where_ $ abstract ^. AbstractIsDraft ==. val v
  case filterAbstractType of
    Nothing -> return ()
    (Just abstractTypeKey) ->
      where_ $ abstractType ^. AbstractTypeId
               ==. val abstractTypeKey

blockUnBlockAbstract :: ConferenceId
                     -> AbstractId
                     -> (AbstractId -> DB ())
                     -> Handler Html
blockUnBlockAbstract confId abstractId action = do
  _ <- requireAdminForConference confId
  runDB $ action abstractId
  redirect $ ConferenceAbstractR confId abstractId

postConferenceBlockAbstractR :: ConferenceId
                             -> AbstractId
                             -> Handler Html
postConferenceBlockAbstractR confId abstractId = do
  blockUnBlockAbstract confId abstractId blockAbstract

postConferenceUnblockAbstractR :: ConferenceId
                               -> AbstractId
                               -> Handler Html
postConferenceUnblockAbstractR confId abstractId = do
  blockUnBlockAbstract confId abstractId unblockAbstract

getConferenceBlockedProposalsR :: ConferenceId -> Handler Html
getConferenceBlockedProposalsR confId = do
  (_, confEntity) <- requireAdminForConference confId
  let getAbstracts =
        select $
          getAbstractsAndAuthorsForConference'' (\ _ _ _ -> return ()) True confId
  abstractList <- runDB getAbstracts
  let ct = encodeCellTable [] (colonnadeAbstracts confId) abstractList
  baseLayout Nothing $ do
    setTitle "Blocked Call for Proposals"
    [whamlet|
<article .grid-container>
  <div .row>
    <div .medium-6 .column>
      ^{renderConferenceWidget confEntity}
    <div .medium-6 .column>
      <a href="@{ConferenceCallForProposalsR confId}">
        Return to unblocked CFPs
  <div .row>
    <div .medium-9 .column>
      <h1>#{length abstractList} blocked abstracts
  <div .row>
    <div .medium-9 .column>
      ^{ct}
|]

pageSize :: Num n => n
pageSize = 20

getConferenceCallForProposalsR :: ConferenceId -> Handler Html
getConferenceCallForProposalsR confId = do
  (_, confEntity) <- requireAdminForConference confId
  abstractTypes <- runDB $ getAbstractTypes confId
  ((cfpFilterFR, filterWidget), _) <- runFormGet (cfpFilterForm abstractTypes)
  cfpFilterF <- case cfpFilterFR of
    FormSuccess cfpFilterF -> return cfpFilterF
    _ -> return dummyCfpFilterForm

  let filters abstractType abstract user =
        genFilterConstraints cfpFilterF abstractType abstract user
      -- | unblocked abstracts only
      getAbstractCnt =
        fmap (fromIntegral . fromMaybe 0 . fmap unValue) . selectFirst $
           getAbstractsAndAuthorsForConferenceCnt filters False confId
      getAbstractPages offs =
        select $
          -- unblocked abstracts only
          getAbstractsAndAuthorsForConferencePage filters False confId (OffsetAndLimit offs pageSize)

  (itemsCnt, abstractPages) <- runDB $ PageUtil.paginateCustom pageSize
                         getAbstractCnt
                         (\pn -> getAbstractPages . fromIntegral . PageUtil.pageOffset pn $ pageSize)
  let abstractCnt = fromIntegral itemsCnt :: Integer
  let abstracts = Page.pageItems (Page.pagesCurrent abstractPages)
  let ct = encodeCellTable [] (colonnadeAbstracts confId) abstracts
      pages = Page.simple pageSize abstractPages
  baseLayout Nothing $ do
    setTitle "Call for Proposals"
    [whamlet|
<article .grid-container>
  <div .row>
    <div .medium-6 .column>
      ^{renderConferenceWidget confEntity}
    <div .medium-6 .column>
      <a href="@{ConferenceBlockedProposalsR confId}">Blocked CFPs
  <div .row>
    <form method="GET" action="@{ConferenceCallForProposalsR confId}">
      ^{filterWidget}
      <input .button type="submit" value="Filter">
  <div .row>
    <div .medium-9 .column>
      <h1>#{abstractCnt} abstracts matching filters
  <div .row>
    <div .medium-9 .column>
      ^{ct}
  <div .row>
    <div .medium-9 .column>
      ^{pages}
|]

colonnadeAbstracts :: ConferenceId
                   -> Colonnade
                      Headed
                      (Entity Abstract, Entity User, Entity AbstractType)
                      (Cell App)
colonnadeAbstracts confId =
  mconcat [
    headed "Title" (cell . titleF . fst')
  , headed "Author Email" (textCell . authorEmailF . snd')
  , headed "Author Name" (textCell . authorNameF . snd')
  , headed "Submitted" (cell . abstractStatusF . fst')
  , headed "Name" (textCell . abstractNameF . thrd')
  , headed "Content" (textCell . contentF . entityVal . fst')
  ]
  where titleF (Entity abstractK abstract) =
          [whamlet|
          <a href=@{ConferenceAbstractR confId abstractK}>
            #{abstractTitle abstract}
          |]
        authorEmailF :: Entity User -> Text
        authorEmailF (Entity _ user) =
          unEmail $ userEmail user
        authorNameF (Entity _ user) =
          userName user
        abstractStatusF (Entity _ abstract) =
          case abstractIsDraft abstract of
            False -> "Submitted"
            True -> "Draft"
        abstractNameF (Entity _ abstractType) =
          abstractTypeName abstractType
        contentF abstract =
          (take 100
            (unMarkdown $ fromMaybe
             (abstractAuthorAbstract abstract)
             (abstractEditedAbstract abstract)
            ))
        fst' (a, _, _) = a
        snd' (_, b, _) = b
        thrd' (_, _, c) = c

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
                          (placeheld "Abstract title:"))
          editedTitle
      <*> areq textareaField (named "abstract-body"
                              (rows "15"
                               (placeheld "Abstract proposal:")))
          editedBody

conferenceAbstractView :: Entity Conference
                       -> Entity Abstract
                       -> Widget
                       -> Enctype
                       -> Handler Html
conferenceAbstractView (Entity confId conference)
  (Entity abstractId Abstract{..}) widget enctype = do
  abstractMarkdown <- renderMarkdown abstractAuthorAbstract
  abstractEditedMarkdown <-
    traverse renderMarkdown abstractEditedAbstract
  baseLayout Nothing $ do
    setTitle "Abstract"
    [whamlet|
<article .grid-container>
  <div .row>
    <div .medium-9 .column>
      <h1>Editing abstract #{tshow (fromSqlKey abstractId)}
      $if abstractBlocked
        <h1>NOTE: THIS ABSTRACT HAS BEEN BLOCKED
      <h3>Conference: #{conferenceName conference}
      <div.row.breathe>
        <div.column>
          <label>Speaker-submitted title:
          <h4>#{abstractAuthorTitle}
      <div.row.breathe>
        <div.column>
          <label>Speaker-submitted abstract:
          <div.callout>
            #{abstractMarkdown}
      $maybe editedTitle <- abstractEditedTitle
        <div.row.breathe>
          <div.column>
            <label>Edited title:
            <h4>#{editedTitle}
      <div #focus>
      $maybe editedMarkdown <- abstractEditedMarkdown
        <div.row.breathe>
          <div.column>
            <label>Edited abstract:
            <div.callout>
              #{editedMarkdown}
      <div.row.breathe>
        <div. .medium-3 .column>
          <form method=POST
                enctype=#{enctype}
                action=@{ConferenceAbstractR confId abstractId}
                #focus>
            ^{widget}
            <input .button type="submit" value="Update abstract">
        $if abstractBlocked
          <div. .medium-3 .column>
            <form method=POST
                  enctype=#{enctype}
                  action=@{ConferenceUnblockAbstractR confId abstractId}
                  #focus>
              <label>
                Warning! This will unblock the abstract and
                reintroduce it for editing and consideration
                for inclusion in the conference.
              <input .button .success type="submit" value="Unblock abstract">
        $else
          <div. .medium-3 .column>
            <form method=POST
                  enctype=#{enctype}
                  action=@{ConferenceBlockAbstractR confId abstractId}
                  #focus>
              <label>
                Warning! This will remove the abstract from
                editing and consideration for inclusion in
                the conference.
              <input .button .alert type="submit" value="Block abstract">
|]

mkAbstractForm :: Abstract -> Form EditedAbstract
mkAbstractForm abstract =
  abstractEditForm
   (Just $ abstractTitle abstract)
   (Textarea . unMarkdown <$>
    (Just $ abstractBody abstract))

getConferenceAbstractR :: ConferenceId -> AbstractId -> Handler Html
getConferenceAbstractR confId abstractId = do
  (_, Entity _ conference) <-
    requireAdminForConference confId
  abstract <- runDBOr404 $ get abstractId
  (widget, enctype) <-
    generateFormPost (mkAbstractForm abstract)
  conferenceAbstractView (Entity confId conference) (Entity abstractId abstract) widget enctype

postConferenceAbstractR :: ConferenceId -> AbstractId -> Handler Html
postConferenceAbstractR confId abstractId = do
  (_, Entity _ conference) <-
    requireAdminForConference confId
  abstract <-
    runDBOr404 $ get abstractId
  ((result, widget), enctype) <-
    runFormPost
      (mkAbstractForm abstract)
  case result of
    FormSuccess (EditedAbstract newTitle newBody) -> do
      let newBodyMd =
            Markdown (unTextarea newBody)
      runDB $ updateAbstract abstractId newTitle newBodyMd
      let newAbstract =
            abstract { abstractEditedTitle = Just newTitle
                     , abstractEditedAbstract = Just newBodyMd
                     }
      conferenceAbstractView
        (Entity confId conference)
        (Entity abstractId newAbstract)
        widget enctype
    _ ->
      conferenceAbstractView
        (Entity confId conference)
        (Entity abstractId abstract)
        widget enctype

-------------------------------------------------------
-- Proof of concept slug versions
-- Note to do a good job these should have used withConferenceSlugRedirect2 and
-- withConferenceSlugStrict2 an I should have changed existing rendering functions
-- (so they know ConferenceSlug for form submission route)
-------------------------------------------------------
getConferenceAbstractPocR :: ConferenceSlug -> AbstractId -> Handler Html
getConferenceAbstractPocR code abstractId =
   withConferenceSlugRedirect (\c -> ConferenceAbstractPocR c abstractId) (flip getConferenceAbstractR abstractId) code

postConferenceAbstractPocR :: ConferenceSlug -> AbstractId -> Handler Html
postConferenceAbstractPocR code abstractId =
   withConferenceSlugStrict (flip postConferenceAbstractR abstractId) code

getUserSearchR :: Text -> Handler A.Value
getUserSearchR query = do
  -- TODO: requireAdmin
  _ <- requireVerifiedUser
  users <-
    runDB $
      select $
      from $ \ user -> do
        -- TODO: lol this is a little gross
        where_ $ ilikeVal user UserEmail (Email query)
        limit 10
        return user
  return $ toJSON $ fmap (unEmail . userEmail . entityVal) users

data SubmittedSurrogateAbstract =
  SubmittedSurrogateAbstract {
    submittedSurrogateAbstractAuthor :: Email
  , submittedSurrogateAbstractTitle :: Text
  , submittedSurrogateAbstractBody :: Textarea
  , submittedSurrogateAbstractType :: AbstractTypeId
  } deriving Show

surrogateAbstractForm' :: [Entity AbstractType]
                       -> Maybe Email
                       -> Maybe Abstract
                       -> Form SubmittedSurrogateAbstract
surrogateAbstractForm' abstractTypes maybeEmail maybeAbstract = do
  let abstractTypeList :: [(Text, AbstractTypeId)]
      abstractTypeList =
        map
        renderAbstractTypeDropdown
        abstractTypes
  renderDivs $
    SubmittedSurrogateAbstract
      <$> areq emailRegisteredField
          (named "abstract-author"
           (placeheld "Abstract author:"))
          maybeEmail
      <*> areq textField (named "abstract-title"
                          (placeheld "Abstract title:"))
          (abstractTitle <$> maybeAbstract)
      <*> areq textareaField (named "abstract-body"
                              (placeheld "Abstract proposal:"))
          (Textarea . unMarkdown . abstractBody <$> maybeAbstract)
      <*> areq (selectFieldList abstractTypeList)
               (named "abstract-type" (placeheld "Abstract type:"))
          (abstractAbstractType <$> maybeAbstract)
  where emailRegisteredField = checkM validateRegistered emailField'
        validateRegistered email = do
          maybeUser <- runDB $ getUserByEmail email
          case maybeUser of
            Nothing ->
              return $
              Left ("No user with this email address exists in the database" :: Text)
            (Just (Entity _ User{..})) ->
              return $ case userVerifiedAt of
                Nothing -> Left "User hasn't yet verified their email address"
                (Just _) -> Right email

surrogateAbstractForm :: [Entity AbstractType] -> Form SubmittedSurrogateAbstract
surrogateAbstractForm abstractTypes =
  surrogateAbstractForm' abstractTypes Nothing Nothing

renderSubmitSurrogateAbstract :: Entity Conference
                              -> Maybe AbstractId
                              -> Widget
                              -> Handler Html
renderSubmitSurrogateAbstract (Entity confId Conference{..})
  maybeAbstractId
  submitSurrogateAbstractForm = do
  render
  where render = baseLayout Nothing $ do
          addStylesheet $ StaticR css_auto_complete_css
          addScript $ StaticR js_auto_complete_min_js
          addScript $ StaticR js_surrogate_autocomplete_js
          [whamlet|
<article .grid-container>
  <div .grid-x .grid-margin-x>
    <div .medium-6 .cell>
      <h5>You are submitting an abstract on behalf of a speaker
      <p>Conference: #{conferenceName}
  <div .grid-x .grid-margin-x>
    <div .medium-6 .cell>
      <form method="POST"
            action="@{ConferenceSurrogateAbstractR confId}">
        ^{submitSurrogateAbstractForm}
        <input .button.success
         type="submit"
         name="submit"
         value="Submit abstract">
|]

getConferenceSurrogateAbstractR :: ConferenceId -> Handler Html
getConferenceSurrogateAbstractR confId = do
  (_, Entity _ conference) <-
    requireAdminForConference confId
  abstractTypes <- runDB $ getAbstractTypes confId
  conf <- runDBOr404 $ get confId
  (abstractWidget, _) <- generateFormPost (surrogateAbstractForm abstractTypes)
  renderSubmitSurrogateAbstract (Entity confId conf) Nothing abstractWidget

data SurrogateSaveException =
  NoUserWithEmailAddress Email
  deriving Show

instance Exception SurrogateSaveException

handleSaveSurrogateAbstract :: ConferenceId
                            -> Bool
                            -> Handler (Either
                                        (Entity Conference, Widget)
                                        (Entity Abstract))
handleSaveSurrogateAbstract confId isDraft = do
  (Entity adminKey _, Entity _ conference) <-
    requireAdminForConference confId
  (conf, abstractTypes) <-
    runDBOr404 $ getConfAndAbstractTypes confId
  ((submittedAbstract, abstractWidget), _) <-
    runFormPost (surrogateAbstractForm abstractTypes)
  case submittedAbstract of
    FormSuccess
      (SubmittedSurrogateAbstract authorEmail title body abstractTypeId) -> do
        abstract <- runDB $ do
          maybeUser <- getUserByEmail authorEmail
          case maybeUser of
            -- We should never reach this,
            -- the form pre-validated the email as being
            -- a verified user already.
            Nothing -> throwIO $ NoUserWithEmailAddress authorEmail
            (Just (Entity userKey _)) -> do
              maybeAbstractType <- get abstractTypeId
              case maybeAbstractType of
                Nothing -> notFound
                (Just _) -> do
                  abstractEntity <-
                    insertEntity
                      (Abstract
                       userKey
                       title abstractTypeId
                       (Markdown (unTextarea body)) Nothing Nothing
                       False isDraft
                      )
                  insertEntity $
                    SurrogateAbstract (entityKey abstractEntity) adminKey
                  return abstractEntity
        return $ Right abstract
    _ -> return $ Left (conf, abstractWidget)

postConferenceSurrogateAbstractR :: ConferenceId -> Handler Html
postConferenceSurrogateAbstractR confId = do
  abstractE <- handleSaveSurrogateAbstract confId False
  case abstractE of
    (Left (conf, widget)) ->
      renderSubmitSurrogateAbstract conf Nothing widget
    (Right (Entity abstractId _)) ->
      redirect (ConferenceAbstractR confId abstractId)
