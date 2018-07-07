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
import Data.Time.Clock
import Database.Esqueleto hiding (selectFirst)
import Database.Esqueleto.Internal.Sql
import Database.Persist.Postgresql (ConnectionString, withPostgresqlPool)
import Model.BCrypt as Export
import Model.Types as Export

-- (PrimaryKey, User)
-- data User = User Email UTCTime
-- (PrimaryKey, User)
-- User -> DB PrimaryKey

-- Int64
-- Primary email

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
  email Email sqltype=varchar(100)
  createdAt UTCTime
  UniqueUserEmail email
  deriving Eq Show

Password sql=passwords
  hash BCrypt
  user UserId

Account sql=accounts

Owner sql=owners
  account AccountId
  user UserId

Admin sql=admins
  account AccountId
  user UserId

Editor sql=editors
  account AccountId
  user UserId

Conference sql=conferences
  owner OwnerId
  name Text
  description Text

AbstractSubmission sql=abstract_submissions
  title Text
  description Text
  author UserId

EditedAbstract sql=edited_abstracts
  abstract AbstractSubmissionId
  editedDescription Text

CustomForm sql=custom_forms
  user UserId

CustomFormInput sql=custom_form_inputs
  form CustomFormId
  name Text
  fieldType Text

CustomFormFilled sql=custom_forms_filled
  parent CustomFormId
  respondee UserId

CustomFormInputFilled sql=custom_form_inputs_filled
  form CustomFormFilledId
  input CustomFormInputId

AbstractType sql=abstract_types
  conference ConferenceId
  name Text
  duration TalkDuration
  deriving Show

Abstract sql=abstracts
  user UserId
  title Text
  authorAbstract Text
  editedAbstract Text Maybe
  deriving Show
|]

dumpMigration :: DB ()
dumpMigration = printMigration migrateAll

runMigrations :: DB ()
runMigrations = runMigration migrateAll

devConn :: ConnectionString
devConn =
  "dbname=moot_dev host=localhost user=moot password=moot port=5432"

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
