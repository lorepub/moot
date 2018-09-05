module Helpers.Handlers where

import Import.NoFoundation

runDBOr404 :: ( YesodPersistBackend site ~ SqlBackend
              , YesodPersist site
              )
           => SqlPersistT (HandlerFor site) (Maybe a)
           -> HandlerFor site a
runDBOr404 dbma = do
  maybeVal <- runDB dbma
  case maybeVal of
    Nothing -> notFound
    (Just val') -> return val'

-- | Allows to use standard ID based semantics while exposing
--   'ConferenceCode' based URL endpoint.  This assumes that the handler logic
--   does not need to form any 'ConferenceCode' specific routes
withConferenceCodeRedirect :: ( YesodPersistBackend site ~ SqlBackend
                              , YesodPersist site
                              )
                           => (ConferenceCode -> Route site)
                           -- ^ use with something like SubmittedAbstractR
                           --   from Routes.hs
                           ->  (ConferenceId -> HandlerFor site a)
                           -- ^ 'ConferenceId' based handler
                           -> ConferenceCode               
                           -> HandlerFor site a
withConferenceCodeRedirect redirectTo handler conferenceCode =
  withConferenceCodeRedirect2 redirectTo (const handler) conferenceCode

-- | in few places we need both 'ConferenceId' and 'ConferenceCode'. 
--   'ConferenceId' is used for DB access, 
--   'ConferenceCode' for using @Route App@ routes on the page.
withConferenceCodeRedirect2 :: ( YesodPersistBackend site ~ SqlBackend
                               , YesodPersist site
                               )
                            => (ConferenceCode -> Route site)
                            -> (ConferenceCode -> ConferenceId -> HandlerFor site a)
                            -> ConferenceCode -> HandlerFor site a
withConferenceCodeRedirect2 redirectTo handler conferenceCode = do
  confIdOrErr <- runDB $ resolveConferenceId conferenceCode
  case confIdOrErr of
     Left SlugMissing -> notFound
     Left (SlugInactive activeCode) -> redirect (redirectTo activeCode)
     Right confId -> handler conferenceCode confId

-- | This version does not redirect, used with POST endpoints which do not need it
withConferenceCodeStrict :: ( YesodPersistBackend site ~ SqlBackend
                            , YesodPersist site
                            )
                         => (ConferenceId -> HandlerFor site a)
                         -> ConferenceCode
                         -> HandlerFor site a
withConferenceCodeStrict handler conferenceCode =
  withConferenceCodeStrict2 (const handler) conferenceCode

withConferenceCodeStrict2 :: ( YesodPersistBackend site ~ SqlBackend
                             , YesodPersist site
                             )
                          => (ConferenceCode -> ConferenceId -> HandlerFor site a)
                          -> ConferenceCode
                          -> HandlerFor site a
withConferenceCodeStrict2 handler conferenceCode = do
  confIdOrErr <- runDB $ resolveConferenceId conferenceCode
  case confIdOrErr of
     Left _ -> notFound
     Right confId -> handler conferenceCode confId
