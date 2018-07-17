{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Types where

import ClassyPrelude.Yesod
import Data.Fixed
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

newtype Token =
  Token { tokenText :: Text }
  deriving (Eq, Show, Read, PathPiece, PersistField, PersistFieldSql)

newtype Minutes =
  Minutes { unMinutes :: Word64 }
  deriving (Eq, Show, PersistField, PersistFieldSql)

renderMinutes :: Minutes -> Text
renderMinutes (Minutes min') =
  let singPlural :: (Eq a, Num a) => a -> Text
      singPlural 1 = ""
      singPlural _ = "s"
  in case quotRem min' 60 of
       -- Well this is plainly bonkers
       (0, 0) -> [st|0 hours, 0 minutes|]
       (0, minutes) -> [st|#{tshow minutes} minute#{singPlural minutes}|]
       (hours, 0) -> [st|#{tshow hours} hour#{singPlural hours}|]
       (hours, minutes) ->
         [st|#{tshow hours} hour#{singPlural hours}, #{tshow minutes} minute#{singPlural minutes}|]

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
