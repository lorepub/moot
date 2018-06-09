module Helpers.Handlers where

import Import

runDBOr404 :: DB (Maybe a) -> Handler a
runDBOr404 dbma = do
  maybeVal <- runDB dbma
  case maybeVal of
    Nothing -> notFound
    (Just val) -> return val
