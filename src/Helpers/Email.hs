module Helpers.Email where

import Import

import qualified Network.Api.Postmark as Postmark
import Text.Blaze.Html.Renderer.Text

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

-- mootPasswordResetEmail :: Text
--                        -> UUID
--                        -> Email
-- mootPasswordResetEmail addressee token =
--   mootDefaultEmail "Moot: Password Reset request" resetHtml [addressee]
--   where resetHtml = toStrict $ renderMarkup [shamlet|
--         <h1>Moot
--         <p>A request to reset your password has
--            been made, <a href="https://mootapp.com/password/reset/#{token}">click here</a>
--            to set a new password. If you did not make this request, ignore it!
--         |]

sendTestEmail :: IO (PostmarkResponse PostmarkError Sent)
sendTestEmail = do
  let settings = postmarkSettings "POSTMARK_API_TEST"
      emailHtml = "<h1>hi</h1>"
  request settings $ Postmark.email (mootDefaultEmail "Test Email" emailHtml ["test@mootapp.com"])

sendEmail' :: PostmarkRequest' x
           -> Handler (PostmarkResponse PostmarkError x)
sendEmail' pr = do
  pmarkSettings <- getsYesod appPostmark
  liftIO $ request pmarkSettings $ pr

sendEmail :: Postmark.Email
          -> Handler (PostmarkResponse PostmarkError Sent)
sendEmail em =
  sendEmail' (Postmark.email em)
