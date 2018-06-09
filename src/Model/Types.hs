{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Types where

import ClassyPrelude.Yesod
-- import Data.Bifunctor
import Data.Fixed
import Text.Email.Validate
import Text.Shakespeare.Text

type ControlIO m = (MonadIO m)

type DBM m a =
  (ControlIO m, MonadThrow m, Monad m) => SqlPersistT m a

type DB a = forall m. DBM m a

type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val))

fetchThingByField
  :: (PersistField typ, DBVal val)
  => EntityField val typ
  -> typ
  -> DB (Maybe (Entity val))
fetchThingByField field u =
  selectFirst [field ==. u] []

data E10

instance HasResolution E10 where
    resolution _ = 10000000000

type Hotness = Fixed E10

type Email = EmailAddress

instance PersistField Email where
  toPersistValue email =
    PersistText $ decodeUtf8 $ toByteString email
  fromPersistValue (PersistText email) =
      first pack
    $ validate (encodeUtf8 email)
  fromPersistValue v =
      Left
    $ [st|Got invalid PersistValue for Email, was: #{tshow v}|]
