{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Types where

import ClassyPrelude.Yesod
-- import Data.Bifunctor
import Data.Fixed
import Data.Time.Clock
import Database.Persist.Sql
import Text.Email.Validate
import Text.Shakespeare.Text

type ControlIO m = (MonadIO m)

type DBM m a =
  (ControlIO m, MonadThrow m, Monad m) => SqlPersistT m a

type DB a = forall m. DBM m a

type DBAll val typ backend =
  ( PersistField typ
  , PersistUniqueRead backend
  , PersistQueryRead backend
  , PersistEntity val
  , BackendCompatible SqlBackend backend
  , BackendCompatible SqlBackend (PersistEntityBackend val)
  )

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

newtype PasswordText =
  PasswordText Text

-- makeNominalDiffTime :: Int64 -> NominalDiffTime
-- makeNominalDiffTime i = secondsToNominalDiffTime (fromIntegral i)

-- unpackNominalDiffTime :: NominalDiffTime -> Int64
-- unpackNominalDiffTime ndt =
--   case nominalDiffTimeToSeconds ndt of
--     (MkFixed i) -> (div (fromIntegral i) (floor $ 10 ^ 12))

-- instance PersistField NominalDiffTime where
--   toPersistValue = PersistInt64 . unpackNominalDiffTime
--   fromPersistValue (PersistInt64 i) =
--     Right $ makeNominalDiffTime i
--   fromPersistValue pv =
--       Left
--     $ "Tried to deserialize nominaldifftime, expected PersistInt64, got: "
--       <> tshow pv

-- instance PersistFieldSql NominalDiffTime where
--   sqlType _ = SqlInt64

newtype Minutes =
  Minutes { unMinutes :: Word64 }
  deriving (Eq, Show, PersistField, PersistFieldSql)

renderMinutes :: Minutes -> Text
renderMinutes (Minutes min) =
  case quotRem min 60 of
    -- Well this is plainly bonkers
    (0, 0) -> [st|0 hours, 0 minutes|]
    (0, minutes) -> [st|#{tshow minutes} minutes|]
    (hours, 0) -> [st|#{tshow hours} hours|]
    (hours, minutes) ->
      [st|#{tshow hours} hours, #{tshow minutes} minutes|]

newtype TalkDuration =
  TalkDuration { unTalkDuration :: Minutes }
  deriving (Eq, Show, PersistField, PersistFieldSql)

renderTalkDuration :: TalkDuration -> Text
renderTalkDuration (TalkDuration minutes) =
  renderMinutes minutes

makeTalkDuration :: Word64 -> TalkDuration
makeTalkDuration = TalkDuration . Minutes

unpackTalkDuration :: TalkDuration -> Word64
unpackTalkDuration = unMinutes . unTalkDuration
