{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model
  ( module Model
  , module Export
  ) where

import ClassyPrelude.Yesod hiding ((==.), hash, on, selectFirst)

import Control.Monad.Logger hiding (LoggingT, runLoggingT)
import Data.UUID
import Database.Esqueleto hiding (selectFirst)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlPool, pgConnStr)
import Model.BCrypt as Export
import Model.Instances ()
import Model.Types as Export
import qualified Settings as S

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
  email Email sqltype=varchar(100)
  name Text sqltype=varchar(100)
  createdAt UTCTime
  verifiedAt UTCTime Maybe
  UniqueUserEmail email
  deriving Eq Show

Password sql=passwords
  hash BCrypt
  user UserId

EmailVerification sql=email_verifications
  uuid UUID
  createdAt UTCTime
  user UserId
  UniqueUserEmailVerification user
  deriving Eq Show

Reset sql=resets
  token Token
  createdAt UTCTime
  user UserId
  UniqueToken token
  deriving Eq Show

Account sql=accounts
  owner OwnerId
  UniqueAccountOwner owner
  deriving Eq Show

Owner sql=owners
  user UserId
  UniqueOwnerUser user
  deriving Eq Show

Admin sql=admins
  user UserId
  conference ConferenceId
  UniqueAdminUserConference user conference
  deriving Eq Show

Editor sql=editors
  user UserId
  conference ConferenceId
  UniqueEditorUserConference user conference
  deriving Eq Show

Conference sql=conferences
  account AccountId
  name Text
  description Text
  cfpWelcome Markdown
  opened UTCTime Maybe
  closed UTCTime Maybe
  deriving Eq Show

CustomForm sql=custom_forms
  user UserId
  deriving Eq Show

CustomFormInput sql=custom_form_inputs
  form CustomFormId
  name Text
  fieldType Text
  deriving Eq Show

CustomFormFilled sql=custom_forms_filled
  parent CustomFormId
  respondee UserId
  deriving Eq Show

CustomFormInputFilled sql=custom_form_inputs_filled
  form CustomFormFilledId
  input CustomFormInputId
  deriving Eq Show

AbstractType sql=abstract_types
  conference ConferenceId
  name Text
  duration TalkDuration
  UniqueAbstractConferenceName conference name
  deriving Eq Show

Abstract sql=abstracts
  user UserId
  authorTitle Text
  abstractType AbstractTypeId
  authorAbstract Markdown
  editedTitle Text Maybe
  editedAbstract Markdown Maybe
  blocked Bool
  isDraft Bool
  deriving Eq Show

SurrogateAbstract sql=surrogate_abstracts
  abstract AbstractId
  surrogate UserId
  deriving Eq Show

Slug sql=conference_slugs
  conference ConferenceId
  code ConferenceSlug
  UniqueSlugCode code
  active Bool
  deriving Eq Show
|]


abstractTitle :: Abstract -> Text
abstractTitle abstract =
  fromMaybe (abstractAuthorTitle abstract) (abstractEditedTitle abstract)

abstractBody :: Abstract -> Markdown
abstractBody abstract =
  fromMaybe (abstractAuthorAbstract abstract) (abstractEditedAbstract abstract)

dumpMigration :: DB ()
dumpMigration = printMigration migrateAll

runMigrations :: DB ()
runMigrations = runMigration migrateAll

devConn :: ConnectionString
devConn = pgConnStr $ S.appDatabaseConf S.compileTimeAppSettings

runDevDB :: DB a -> IO a
runDevDB a =
  runNoLoggingT $
    withPostgresqlPool devConn 3
      $ \pool -> liftIO $ runSqlPersistMPool a pool

runDevDBV :: DB a -> IO a
runDevDBV a =
  runStdoutLoggingT $
    withPostgresqlPool devConn 3
      $ \pool -> liftIO $ runSqlPersistMPool a pool
