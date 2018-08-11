module Helpers.Pagination
  ( module Helpers.Pagination
  , module Export
  ) where

import Import hiding (on)

import Data.Function (on)
import Data.List (genericLength, nubBy)
import qualified Data.Text as T
import Network.URI.Encode (encodeText)
import Yesod.Paginator.Pages as Export
import Yesod.Paginator.Widgets as Export
import Model.API

-- | Paginate out of a persistent database
-- selectPaginated
--     :: ( PersistEntity val
--        , PersistEntityBackend val ~ BaseBackend (YesodPersistBackend site)
--        , PersistQuery (YesodPersistBackend site)
--        , Yesod site
--        )
--     => PerPage
--     -> [Filter val]
--     -> [SelectOpt val]
--     -> YesodDB site (Pages (Entity val))
-- selectPaginated per filters options =
--     selectPaginated' per filters options =<< lift getCurrentPage

-- | A version where the current page is given
--
-- This can be used to avoid the Yesod context
--
-- selectPaginated'
--     :: ( MonadIO m
--        )
--     => PerPage
--     -> [Filter record]
--     -> [SelectOpt record]
--     -> PageNumber
--     -> query
--     -> ReaderT backend m (Pages (Entity record))
selectPaginated' :: ( SqlSelect a r
                    , MonadIO m
                    , BackendCompatible SqlBackend backend
                    , PersistQueryRead backend
                    , PersistUniqueRead backend
                    )
                 => PerPage
                 -> PageNumber
                 -> SqlQuery a
                 -> ReaderT backend m (Pages r)
selectPaginated' per p query = do
  undefined
  -- select query
  -- (toPages p per <$> (fromIntegral <$> count filters) <*>)
    
        -- filters
        -- (options
        -- <> [ OffsetBy $ fromIntegral $ pageOffset p per
        --    , LimitTo $ fromIntegral per
        --    ]
        -- )

-- getUpdateGetParams :: WidgetFor site (PageNumber -> [(Text, Text)])
-- getUpdateGetParams = do
--     params <- handlerToWidget $ reqGetParams <$> getRequest
--     pure $ \number -> nubOn fst $ [("p", tshow number)] <> params
-- nubOn :: Eq b => (a -> b) -> [a] -> [a]
-- nubOn f = nubBy ((==) `on` f)

-- renderGetParams :: [(Text, Text)] -> Text
-- renderGetParams [] = ""
-- renderGetParams ps = "?" <> T.intercalate "&" (map renderGetParam ps)
--     where renderGetParam (k, v) = encodeText k <> "=" <> encodeText v

-- getBalancedPages :: Natural -> Pages a -> ([PageNumber], [PageNumber])
-- getBalancedPages elements pages =
--     if genericLength nextPages >= (elements `div` 2)
--         then (prevPagesNaive, nextPages)
--         else (prevPagesCalcd, nextPages)
--   where
--     nextPages = takeNextPages (elements - genericLength prevPagesNaive - 1) pages
--     prevPagesNaive = takePreviousPages (elements `div` 2) pages
--     prevPagesCalcd = takePreviousPages (elements - genericLength nextPages - 1) pages

-- getUpdateGetParams :: WidgetFor site (PageNumber -> [(Text, Text)])
-- getUpdateGetParams = do
--     params <- handlerToWidget $ reqGetParams <$> getRequest
--     pure $ \number -> nubOn fst $ [("p", tshow number)] <> params

-- paginate :: Natural -> Pages a -> Widget
-- paginate elements pages = do
--   updateGetParams <- getUpdateGetParams
--   let (prevPages, nextPages) = getBalancedPages elements pages
--       mPrevPage = getPreviousPage pages
--       mNextPage = getNextPage pages
--   [whamlet|$newline never
--         <ul .pagination>
--             $maybe prevPage <- mPrevPage
--                 <li .prev>
--                     <a href=#{renderGetParams $ updateGetParams prevPage}>«
--             $nothing
--                 <li .prev .disabled>
--                     <a>«
--             $forall number <- prevPages
--                 <li .prev >
--                     <a href=#{renderGetParams $ updateGetParams number}>#{number}
--             $with number <- pageNumber $ pagesCurrent pages
--                 <li .active .disabled>
--                     <a>#{number}
--             $forall number <- nextPages
--                 <li .next>
--                     <a href=#{renderGetParams $ updateGetParams number}>#{number}
--             $maybe nextPage <- mNextPage
--                 <li .next>
--                     <a href=#{renderGetParams $ updateGetParams nextPage}>»
--             $nothing
--                 <li .next .disabled>
--                     <a>»
--         |]
