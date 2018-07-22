module Handler.Abstract where

import Import

import Helpers.Forms
import Helpers.Handlers
import Helpers.Views

abstractBlurbMarkdown = ""
abstractTypes = []

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
    submittedAbstractEmail :: Email
  , submittedAbstractPassword :: Text
  , submittedAbstractSpeaker :: Text
  , submittedAbstractTitle :: Text
  , submittedAbstractBody :: Textarea
  , submittedAbstractType :: AbstractTypeId
  } deriving Show

renderAbstractTypeDropdown :: Entity AbstractType -> (Text, AbstractTypeId)
renderAbstractTypeDropdown (Entity abstractTypeK abstractType) =
  let -- durationLabel =
      --   abstractTypeDuration abstractType
      prettyAbstractLabel =
        renderAbstractType abstractType
        -- [st|#{abstractTypeName abstractType} (#{durationLabel})|]
  in (prettyAbstractLabel, abstractTypeK) 

abstractForm :: [Entity AbstractType] -> Form SubmittedAbstract
abstractForm abstractTypes = do
  let abstractTypeList :: [(Text, AbstractTypeId)]
      abstractTypeList =
        map
        renderAbstractTypeDropdown
        abstractTypes
  renderDivs $
      SubmittedAbstract
      <$> areq emailField' (named "email"
                           (placeheld "Email:")) Nothing
      <*> areq passwordField (named "password"
                              (placeheld "Password: ")) Nothing
      <*> areq textField (named "speaker-name"
                          (placeheld "Speaker's name:")) Nothing
      <*> areq textField (named "abstract-title"
                          (placeheld "Abstract title:")) Nothing
      <*> areq textareaField (named "abstract-body"
                              (placeheld "Abstract proposal:")) Nothing
      <*> areq (selectFieldList abstractTypeList)
               (named "abstract-type" (placeheld "Abstract type:")) Nothing

renderSubmitAbstract :: Int64
                     -> Widget
                     -> Handler Html
renderSubmitAbstract confId submitAbstractForm =
  baseLayout Nothing $ [whamlet|
<article .grid-container>
  <div .grid-x .grid-margin-x>
    <div .medium-6 .cell>
      <form method="POST"
            action="@{SubmitAbstractR confId}">
        ^{submitAbstractForm}
        <input .button type="submit" value="Submit abstract">
|]

getSubmitAbstractR :: Int64 -> Handler Html
getSubmitAbstractR conferenceId = do
  abstractTypes <- runDB $ abstractTypesForConference (toSqlKey conferenceId)
  (widget, _) <- generateFormPost (abstractForm abstractTypes)
  renderSubmitAbstract conferenceId widget

postSubmitAbstractR :: Int64 -> Handler Html
postSubmitAbstractR conferenceId = do
  let confK = toSqlKey conferenceId
  abstractTypes <- runDBOr404 $ do
    maybeConf <- get confK
    case maybeConf of
      Nothing -> return Nothing
      (Just _) -> do
        fmap Just $ abstractTypesForConference confK

  ((result, widget), _) <- runFormPost (abstractForm abstractTypes)
  case result of
    FormSuccess
      (SubmittedAbstract email password name title body abstractTypeId) -> do
        abstractKey <- runDB $ do
          user <- createUser email password
          lift $ setUserSession (entityKey user) True
          maybeAbstractType <- get abstractTypeId
          case maybeAbstractType of
            Nothing -> undefined
            (Just _) -> do
              insert
                (Abstract
                 confK (entityKey user)
                 title abstractTypeId
                 (unTextarea body) Nothing)
        redirect (SubmittedAbstractR conferenceId)
    _ -> renderSubmitAbstract conferenceId widget

getSubmittedAbstractR :: Int64 -> Handler Html
getSubmittedAbstractR confId = do
  let confK :: ConferenceId
      confK = toSqlKey confId
  _ <- runDBOr404 $ get confK
  baseLayout Nothing $ [whamlet|
<article .grid-container>
  <div .grid-x .grid-margin-x>
    <div .medium-6 .cell>
      <h3>You have successfully submitted your abstract!
      <p>
        Would you like to <a href="@{SubmitAbstractR confId}">submit another?</a>
|]
