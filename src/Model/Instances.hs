{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Instances where

import ClassyPrelude.Yesod

import Data.ByteString.Char8 as B8
import           Data.UUID   (UUID)
import qualified Data.UUID as UUID
import Database.Persist.Sql

instance PersistField UUID where
  toPersistValue u = PersistDbSpecific . B8.pack . UUID.toString $ u
  fromPersistValue (PersistDbSpecific t) =
    case UUID.fromString $ B8.unpack t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  fromPathPiece t = UUID.fromText t
  toPathPiece s = UUID.toText s
