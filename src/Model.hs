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
import Database.Esqueleto hiding (selectFirst)
import Database.Esqueleto.Internal.Sql
import Database.Persist.Postgresql (ConnectionString, withPostgresqlPool)
import Model.BCrypt as Export
import Model.Types as Export

--     t.index ["mailing_list_mode"], name: "mailing_list_enabled"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
  email Text sqltype=varchar(100)
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
|]

-- role subsumption?
-- getRolesForUser :: UserId -> DB (Maybe [Roles])
-- getRolesForUser userKey = undefined

selectFirst :: ( SqlSelect a r
               , MonadIO m
               )
            => SqlQuery a
            -> SqlReadT m (Maybe r)
selectFirst query = do
  res <- select query
  case res of
    (x : _) -> return (Just x)
    _ -> return Nothing

getUserPassword :: Text
                -> DB (Maybe
                       ( Entity User
                       , Entity Password))
getUserPassword email = do
  maybeUser <- getUserByEmail email
  case maybeUser of
    Nothing -> return Nothing
    (Just user) -> do
      maybePassword <-
        selectFirst $
          from $ \password -> do
            where_ (password ^. PasswordUser
                      ==. val (entityKey user))
            return password
      case maybePassword of
        Nothing -> return Nothing
        (Just password) ->
          return $ Just (user, password)


getUserByEmail :: Text -> DB (Maybe (Entity User))
getUserByEmail email =
  getUserBy UserEmail email

getUserBy :: (PersistField a)
          => EntityField User a
          -> a
          -> DB (Maybe (Entity User))
getUserBy field value =
  selectFirst $
  from $ \user -> do
  where_ (user ^. field ==. val value)
  return user

defaultCreateUser :: Text
                  -> IO User
defaultCreateUser userEmail = do
  t <- getCurrentTime
  let userCreatedAt = t
  return $ User{..}

createUser :: Text -> Text -> DB (Entity User)
createUser email pass = do
  newUser <- liftIO $ defaultCreateUser email
  userId <- insert newUser
  hash <- liftIO $ hashPassword pass
  _ <- insert (Password hash userId)
  return (Entity userId newUser)

dumpMigration :: DB ()
dumpMigration = printMigration migrateAll

runMigrations :: DB ()
runMigrations = runMigration migrateAll

devConn :: ConnectionString
devConn =
  "dbname=moot_dev host=localhost user=postgres password=password port=5432"

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
