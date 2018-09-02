module Helpers.Email where

import Import

import qualified Network.Api.Postmark as Postmark
import Text.Blaze.Renderer.Text

type PostmarkToken = Text

postmarkSettings :: PostmarkToken -> PostmarkSettings
postmarkSettings apiToken =
  PostmarkSettings "https://api.postmarkapp.com" apiToken

mootDefaultEmail :: Text
                 -> Text
                 -> [Text]
                 -> Postmark.Email
mootDefaultEmail subject html addressees =
   defaultEmail {
          emailFrom = "support@mootapp.com"
        , emailTo = addressees -- ["test@email.com"]
        , emailSubject = subject -- "demo, yes it really is a demo"
        , emailTag = Nothing
        , emailHtml = Just html
        , emailReplyTo = "support@mootapp.com"
        }

mootVerificationEmail :: (Route App -> Text)
                      -> Text
                      -> UUID
                      -> Postmark.Email
mootVerificationEmail urlRenderer addressee token =
  mootDefaultEmail "Moot: Email Verification required" emailHtml [addressee]
  where emailHtml = toStrict $ renderMarkup $ [shamlet|
        <h1>Moot
        <p>Thanks for signing up with Moot!
        <p>To begin using Moot, <a href="#{urlRenderer $ VerifyR token}">click here to activate your account.</a> If you did not create this account, tell us!
        |]

-- mootPasswordResetEmail :: Text
--                        -> UUID
--                        -> Postmark.Email
-- mootPasswordResetEmail addressee token =
--   mootDefaultEmail "Moot: Password Reset request" resetHtml [addressee]
--   where resetHtml = toStrict $ renderMarkup [shamlet|
--         <h1>Moot
--         <p>A request to reset your password has
--            been made, <a href="https://mootapp.com/password/reset/#{token}">click here</a>
--            to set a new password. If you did not make this request, ignore it!
--         |]

sendTestEmail' :: Text -> Text -> IO (PostmarkResponse PostmarkError Sent)
sendTestEmail' apiToken destination = do
  let settings = postmarkSettings apiToken
      emailHtml = "<h1>hi</h1>"
  request settings $ Postmark.email (mootDefaultEmail "Test Email" emailHtml [destination])

sendTestEmail :: Text -> IO (PostmarkResponse PostmarkError Sent)
sendTestEmail destination = do
  let settings = postmarkSettings "POSTMARK_API_TEST"
      emailHtml = "<h1>hi</h1>"
  request settings $ Postmark.email (mootDefaultEmail "Test Email" emailHtml [destination])

sendEmail' :: PostmarkRequest' x
           -> Handler (PostmarkResponse PostmarkError x)
sendEmail' pr = do
  pmarkSettings <- getsYesod appPostmark
  liftIO $ request pmarkSettings $ pr

sendEmail :: Postmark.Email
          -> Handler (PostmarkResponse PostmarkError Sent)
sendEmail em =
  sendEmail' (Postmark.email em)
