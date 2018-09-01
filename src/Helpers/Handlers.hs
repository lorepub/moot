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
