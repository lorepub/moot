module Handler.Abstract where

import Import

import Data.Bitraversable
import Handler.Auth
import Helpers.Forms
import Helpers.Handlers
import Helpers.Views

-- email address
-- twitter handle
-- phone number (Maybe)
-- country

-- previous talks given
-- URL to previous talk(s)

----- 2017 -----

-- Leap workshop, hop workshop, educational session
-- Title
-- Intro, what the talk is about
-- Relevancy. Why is this session relevant to a professional software developer?
-- Concepts. What concepts will developers learn from the session?
-- Skills. What concrete skills will developers acquire from the session?
-- Outline. Please create a brief outline how you intend to structure the session.
-- Pitch. What is the main reason developers should come to your session instead of other ones?
-- Background Requirements. If your session is on statically-typed, category-theoretic functional programming (Haskell, PureScript, Scala, etc.), please choose the category that best matches the contents of your session, such that people who are actively learning or mostly know the category contents will understand your session.
-- Note: These topic categories are based on LOFP — please see here for more details.
-- If relevant, what language(s) will you use to provide code samples?

-- Denovo
-- title
-- intro
-- Novelty. What is the core idea of your original research / novel solution?
-- Competition. What are similar or related approaches to the problem that you are solving? Citations welcome.
-- Differentiation. Why do other solutions compare unfavorably with your own work (to the extent they do)?
-- Relevancy. Why is this session relevant to a professional software developer?
-- Benefits. How will this session help developers to better accomplish their job?
-- Concepts. What concepts will developers learn from the session?
-- Skills. What concrete skills will developers acquire from the session?
-- Outline. Please create a brief outline how you intend to structure the session.
-- Pitch. What is the main reason developers should come to your session instead of other ones?
-- Background Requirements. If your session is on statically-typed, category-theoretic functional programming (Haskell, PureScript, Scala, etc.), please choose the category that best matches the contents of your session, such that people who are actively learning or mostly know the category contents will understand your session.
-- Note: These topic categories are based on LOFP — please see here for more details.
-- If relevant, what language(s) will you use to provide code samples?

-- Inspire
-- title
-- intro
-- Takeaway. What is the ONE takeaway for developers who attend your session?
-- Inspiration. In what way do you hope your session will inspire developers?
-- Entertainment. If relevant, in what way do you hope your session will entertain developers?
-- Relevancy. Why is this session relevant to a professional software developer?
-- Benefits. How will the subject matter you're covering help developers to better accomplish their job?
-- Outline. Please create a brief outline how you intend to structure the session.
-- Pitch. What is the main reason developers should come to your session instead of other ones?
-- If relevant, what language(s) will you use to provide code samples?

-- Keynote
-- title
-- intro
-- Inspiration. In what way do you hope your session will inspire developers?
-- Entertainment. If relevant, in what way do you hope your session will entertain developers?
-- Relevancy. Why is this session relevant to a professional software developer?
-- Takeaway. What is the ONE takeaway for developers who attend your session?
-- Discussion. What sorts of hallway discussions do you hope developers will have after attending your session?
-- Outline. Please create a brief outline how you intend to structure the session.
-- Pitch. What is the main reason developers should come to your session instead of other ones?
-- Background Requirements. If your session is on statically-typed, category-theoretic functional programming (Haskell, PureScript, Scala, etc.), please choose the category that best matches the contents of your session, such that people who are actively learning or mostly know the category contents will understand your session.
-- Note: These topic categories are based on LOFP — please see here for more details.
-- If relevant, what language(s) will you use to provide code samples?

----- 2016 -----

-- Title
-- Dropdown menu of topics
-- Downdown of submission type
-- Abstract summary, ~300 words
-- Relevancy. Why is this session relevant to a professional software developer?
-- Country
-- Travel assistance, checkbox: I am financially unable to speak at LambdaConf without travel assistance I would like travel assistance if it's available I do not require travel assistance to speak at LambdaConf

-- Dryfta
-- allowed you to assign roles as a primary or assistant co-author/presenter

-- Speaker + co-speaker integration

-- Form builder

-- [MVP: minimal version of the pipeline]
-- CFP collected, blind edit, blind review/rate, schedule.

-- Non-MVP follow-up questions for accepted speakers
-- Dietary preferences, kids/childcare

-- Skip dropdown toggle
-- Segment, form for a particular group
-- View all of the results submitted, blinded editor edits submissions
-- Export, or blinded reviews review in app
-- Ranking, accept or reject
-- Submitter communications/emails
-- Schedule creation

-- Pre-edit, post-edit cloak

-- Pre-edit cloaked view (committee role)
-- abstract title
-- abstract type

-- Post-edit cloaked view
-- abstract title
-- abstract proposal
-- abstract type

-- Conference cloning

data SubmittedAbstract =
  SubmittedAbstract {
    submittedAbstractTitle :: Text
  , submittedAbstractBody :: Textarea
  , submittedAbstractType :: AbstractTypeId
  } deriving Show

abstractForm' :: [Entity AbstractType]
              -> Maybe Abstract
              -> Form SubmittedAbstract
abstractForm' abstractTypes maybeAbstract = do
  let abstractTypeList :: [(Text, AbstractTypeId)]
      abstractTypeList =
        map
        renderAbstractTypeDropdown
        abstractTypes
  renderDivs $
    SubmittedAbstract
      <$> areq textField (named "abstract-title"
                          (placeheld "Abstract title:"))
          (abstractTitle <$> maybeAbstract)
      <*> areq textareaField (named "abstract-body"
                              (placeheld "Abstract proposal:"))
          (Textarea . unMarkdown . abstractBody <$> maybeAbstract)
      <*> areq (selectFieldList abstractTypeList)
               (named "abstract-type" (placeheld "Abstract type:"))
          (abstractAbstractType <$> maybeAbstract)

abstractForm :: [Entity AbstractType] -> Form SubmittedAbstract
abstractForm abstractTypes =
  abstractForm' abstractTypes Nothing

renderSubmitAbstract :: Entity Conference
                     -> Maybe AbstractId
                     -> Widget
                     -> Handler Html
renderSubmitAbstract (Entity confId Conference{..})
  maybeAbstractId
  submitAbstractForm = do
  welcomeMarkdown <- renderMarkdown conferenceCfpWelcome
  case maybeAbstractId of
    Nothing ->
      render welcomeMarkdown renderNew
    (Just abstractId) ->
      render welcomeMarkdown (renderUpdateExistingDraft abstractId)
  where render welcomeMarkdown rest = baseLayout Nothing $ [whamlet|
<article .grid-container>
  <div .grid-x .grid-margin-x>
    <div .medium-6 .cell>
      #{welcomeMarkdown}
      <form method="POST"
            action="@{SubmitAbstractR confId}">
        ^{submitAbstractForm}
        <input .button.success
         type="submit"
         name="submit"
         value="Submit abstract">
        ^{rest}
|]
        renderNew = [whamlet|
         <input .button
          type="submit"
          name="submit"
          formaction="@{SubmitAbstractDraftR confId}"
          value="Save draft">
         <p>
           If you submit the abstract now, that will make it the final version
           and you will not be able to make further changes. If you wish to save
           your abstract and finish writing it later, click "Save draft."
        |]
        renderUpdateExistingDraft abstractId = [whamlet|
        <input .button
         type="submit"
         name="submit"
         formaction="@{AbstractDraftR confId abstractId}"
         value="Save draft">
        <p>
          You are viewing your draft submission to this conference. To save your
          submission as a draft and continue editing later, click "Save draft."
          If you are done with this submission and ready to submit,
          click "Submit abstract."
        |]

getSubmitAbstractR :: ConferenceId -> Handler Html
getSubmitAbstractR conferenceId = do
  user <- requireVerifiedUser
  abstractTypes <- runDB $ getAbstractTypes conferenceId
  conf <- runDBOr404 $ get conferenceId
  (abstractWidget, _) <- generateFormPost (abstractForm abstractTypes)
  renderSubmitAbstract (Entity conferenceId conf) Nothing abstractWidget

getConfAndAbstractTypes :: ConferenceId
                        -> DB (Maybe (Entity Conference, [Entity AbstractType]))
getConfAndAbstractTypes confId = do
  maybeConf <- getConference confId
  case maybeConf of
    Nothing -> return Nothing
    (Just conf) -> do
      abstractTypes <- getAbstractTypes confId
      return $ Just $ (conf, abstractTypes)

handleUpdateAbstract :: ConferenceId
                     -> AbstractId
                     -> Bool
                     -> Handler (Either
                                 (Entity Conference, Widget)
                                 (Entity Abstract))
handleUpdateAbstract confId abstractId isDraft = do
  user <- requireVerifiedUser
  (conf, abstractTypes) <-
    runDBOr404 $ getConfAndAbstractTypes confId
  ((submittedAbstract, abstractWidget), _) <-
    runFormPost (abstractForm abstractTypes)
  case submittedAbstract of
    FormSuccess
      (SubmittedAbstract title body abstractTypeId) -> do
        abstract <- runDB $ do
          abstractTuple <-
            bisequenceA (get abstractTypeId, get abstractId)
          case bisequenceA abstractTuple of
            Nothing -> notFound
            (Just (abstractType, abstract)) -> do
              if abstractUser abstract == entityKey user
                then do
                  updateAbstract abstractId title body abstractTypeId
                  return (Entity abstractId abstract)
                else lift $ permissionDenied "This is not your abstract"
        return $ Right abstract
    _ -> return $ Left (conf, abstractWidget)
  where updateAbstract :: AbstractId
                       -> Text
                       -> Textarea
                       -> AbstractTypeId
                       -> DB ()
        updateAbstract abstractId title body abstractTypeId =
          update $ \a -> do
             set a [ AbstractAuthorTitle =. val title
                   , AbstractAuthorAbstract =. val (Markdown (unTextarea body))
                   , AbstractAbstractType =. val abstractTypeId
                   ]
             where_ (a ^. AbstractId ==. val abstractId)

handleSaveAbstract :: ConferenceId
                   -> Bool
                   -> Handler (Either
                               (Entity Conference, Widget)
                               (Entity Abstract))
handleSaveAbstract confId isDraft = do
  user <- requireVerifiedUser
  (conf, abstractTypes) <-
    runDBOr404 $ getConfAndAbstractTypes confId
  ((submittedAbstract, abstractWidget), _) <-
    runFormPost (abstractForm abstractTypes)
  case submittedAbstract of
    FormSuccess
      (SubmittedAbstract title body abstractTypeId) -> do
        abstract <- runDB $ do
          maybeAbstractType <- get abstractTypeId
          case maybeAbstractType of
            Nothing -> notFound
            (Just _) -> do
              insertEntity
                (Abstract
                 (entityKey user)
                 title abstractTypeId
                 (Markdown (unTextarea body)) Nothing Nothing
                 False isDraft
                )
        return $ Right abstract
    _ -> return $ Left (conf, abstractWidget)

postSubmitAbstractR :: ConferenceId -> Handler Html
postSubmitAbstractR confId = do
  abstractE <- handleSaveAbstract confId False
  case abstractE of
    (Left (conf, widget)) ->
      renderSubmitAbstract conf Nothing widget
    (Right _) ->
      redirect (SubmittedAbstractR confId)

postSubmitAbstractDraftR :: ConferenceId -> Handler Html
postSubmitAbstractDraftR confId = do
  abstractE <- handleSaveAbstract confId True
  case abstractE of
    (Left (conf, widget)) ->
      renderSubmitAbstract conf Nothing widget
    (Right abstract) ->
      redirect (AbstractDraftR confId (entityKey abstract))

getAbstractDraftR :: ConferenceId -> AbstractId -> Handler Html
getAbstractDraftR confId abstractId = do
  user <- requireVerifiedUser
  conf <- runDBOr404 $ get confId
  (abstractTypes, maybeAbstract) <- runDB $ do
    abstractTypes <- getAbstractTypes confId
    maybeAbstract <- get abstractId
    return (abstractTypes, maybeAbstract)
  (abstractWidget, _) <- generateFormPost (abstractForm' abstractTypes maybeAbstract)
  renderSubmitAbstract (Entity confId conf) (Just abstractId) abstractWidget

postAbstractDraftR :: ConferenceId -> AbstractId -> Handler Html
postAbstractDraftR confId abstractId = do
  abstractE <- handleUpdateAbstract confId abstractId True
  case abstractE of
    (Left (conf, widget)) ->
      renderSubmitAbstract conf (Just abstractId) widget
    (Right _) ->
      getAbstractDraftR confId abstractId

getSubmittedAbstractR :: ConferenceId -> Handler Html
getSubmittedAbstractR confId = do
  _ <- runDBOr404 $ get confId
  baseLayout Nothing $ [whamlet|
<article .grid-container>
  <div .grid-x .grid-margin-x>
    <div .medium-6 .cell>
      <h3>You have successfully submitted your abstract!
      <p>
        Would you like to <a href="@{SubmitAbstractR confId}">submit another?</a>
|]
