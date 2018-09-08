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
--   'ConferenceSlug' based URL endpoint.  This assumes that the handler logic
--   does not need to form any 'ConferenceSlug' specific routes
withConferenceSlugRedirect :: ( YesodPersistBackend site ~ SqlBackend
                              , YesodPersist site
                              )
                           => (ConferenceSlug -> Route site)
                           -- ^ use with something like SubmittedAbstractR
                           --   from Routes.hs
                           ->  (ConferenceId -> HandlerFor site a)
                           -- ^ 'ConferenceId' based handler
                           -> ConferenceSlug
                           -> HandlerFor site a
withConferenceSlugRedirect redirectTo handler conferenceCode =
  withConferenceSlugRedirect2 redirectTo (const handler) conferenceCode

-- | in few places we need both 'ConferenceId' and 'ConferenceSlug'.
--   'ConferenceId' is used for DB access,
--   'ConferenceSlug' for using @Route App@ routes on the page.
withConferenceSlugRedirect2 :: ( YesodPersistBackend site ~ SqlBackend
                               , YesodPersist site
                               )
                            => (ConferenceSlug -> Route site)
                            -> (ConferenceSlug -> ConferenceId -> HandlerFor site a)
                            -> ConferenceSlug -> HandlerFor site a
withConferenceSlugRedirect2 redirectTo handler conferenceCode = do
  confIdOrErr <- runDB $ resolveConferenceId conferenceCode
  case confIdOrErr of
     Left SlugMissing -> notFound
     Left (SlugInactive activeCode) -> redirect (redirectTo activeCode)
     Right confId -> handler conferenceCode confId

-- | This version does not redirect, used with POST endpoints which do not need it
withConferenceSlugStrict :: ( YesodPersistBackend site ~ SqlBackend
                            , YesodPersist site
                            )
                         => (ConferenceId -> HandlerFor site a)
                         -> ConferenceSlug
                         -> HandlerFor site a
withConferenceSlugStrict handler conferenceCode =
  withConferenceSlugStrict2 (const handler) conferenceCode

withConferenceSlugStrict2 :: ( YesodPersistBackend site ~ SqlBackend
                             , YesodPersist site
                             )
                          => (ConferenceSlug -> ConferenceId -> HandlerFor site a)
                          -> ConferenceSlug
                          -> HandlerFor site a
withConferenceSlugStrict2 handler conferenceCode = do
  confIdOrErr <- runDB $ resolveConferenceId conferenceCode
  case confIdOrErr of
     Left _ -> notFound
     Right confId -> handler conferenceCode confId
