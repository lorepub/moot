module Helpers.Handlers where

import Import

runDBOr404 :: DB (Maybe a) -> Handler a
runDBOr404 dbma = do
  maybeVal <- runDB dbma
  case maybeVal of
    Nothing -> notFound
    (Just val') -> return val'

-- | Allows to use standard ID based semantics while exposing
--   'ConferenceCode' based URL endpoint.  This assumes that the handler logic
--   does not need to form any 'ConferenceCode' specific routes
withConferenceCodeRedirect :: (ConferenceCode -> Route App) -- ^ use with something like SubmittedAbstractR from Routes.hs
                            ->  (ConferenceId -> Handler a) -- ^ 'ConferenceId' based handler
                            -> ConferenceCode               
                            -> Handler a
withConferenceCodeRedirect redirectTo handler conferenceCode = withConferenceCodeRedirect2 redirectTo (const handler) conferenceCode

-- | in few places we need both 'ConferenceId' and 'ConferenceCode'. 
--   'ConferenceId' is used for DB access, 
--   'ConferenceCode' for using @Route App@ routes on the page.
withConferenceCodeRedirect2 :: (ConferenceCode -> Route App) ->  (ConferenceCode -> ConferenceId -> Handler a) -> ConferenceCode -> Handler a
withConferenceCodeRedirect2 redirectTo handler conferenceCode = do
              confIdOrErr <- runDB $ resolveConferenceId conferenceCode
              case confIdOrErr of
                 Left SlugMissing -> notFound
                 Left (SlugInactive activeCode) -> redirect (redirectTo activeCode)
                 Right confId -> handler conferenceCode confId

-- | This version does not redirect, used with POST endpoints which do not need it
withConferenceCodeStrict :: (ConferenceId -> Handler a) -> ConferenceCode -> Handler a
withConferenceCodeStrict handler conferenceCode = withConferenceCodeStrict2 (const handler) conferenceCode

withConferenceCodeStrict2 :: (ConferenceCode -> ConferenceId -> Handler a) -> ConferenceCode -> Handler a
withConferenceCodeStrict2 handler conferenceCode = do
              confIdOrErr <- runDB $ resolveConferenceId conferenceCode
              case confIdOrErr of
                 Left _ -> notFound
                 Right confId -> handler conferenceCode confId
