module Helpers.PaginateUtil (
   paginateCustom
  , pageOffset
) 
where

import Yesod.Paginator.Prelude
import Yesod.Core
import Yesod.Paginator.Pages
import Yesod.Persist.Core


-- | Utility for better custom pagination allowing only one runDB.
-- mimics outstanding pull request against Paginator project 
paginateCustom :: (MonadTrans t, MonadHandler m, Monad (t m)) =>
                     PerPage
                     -> t m ItemsCount -> (PageNumber -> t m [a]) -> t m (ItemsCount, Pages a)
paginateCustom per getcount getitems = do
     cnt <- getcount 
     p <- lift getCurrentPage
     pitems <- getitems p
     pure $ (cnt, toPages p per cnt pitems)

getCurrentPage :: MonadHandler m => m PageNumber
getCurrentPage = fromMaybe 1 . go <$> lookupGetParam "p"
  where
    go :: Maybe Text -> Maybe PageNumber
    go mp = readIntegral . unpack =<< mp