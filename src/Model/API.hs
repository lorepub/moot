module Model.API
  ( module Model.API
  , module Export
  ) where

import ClassyPrelude.Yesod hiding ((==.), hash, on, selectFirst)

import Database.Esqueleto hiding (selectFirst)
import Database.Esqueleto.Internal.Sql
import Database.Esqueleto.Internal.Sql as Export (SqlQuery)

import Model as Export

getOwnerForUser :: UserId -> DB (Maybe (Entity Owner))
getOwnerForUser userId = getRecByField OwnerUser userId

getAccountByOwner :: OwnerId -> DB (Maybe (Entity Account))
getAccountByOwner ownerId = getRecByField AccountOwner ownerId

getAccountByUser :: UserId -> DB (Maybe (Entity Account))
getAccountByUser userId = do
  selectFirst $
    from $ \(account `InnerJoin` owner) -> do
      on (account ^. AccountOwner ==. owner ^. OwnerId)
      where_ (owner ^. OwnerUser ==. val userId)
      return account

getRecsByField' :: ( DBAll val typ backend
                  , MonadIO m
                 )
               => EntityField val typ
               -> (SqlExpr (Entity val) -> SqlQuery a)
               -> typ
               -> ReaderT backend m [Entity val]
getRecsByField' f w x =
  select $
  from $ \ r -> do
    where_ (r ^. f ==. val x)
    void (w r)
    return r

getRecsByField :: ( DBAll val typ backend
                 , MonadIO m
                 )
              => EntityField val typ
              -> typ
              -> ReaderT backend m [Entity val]
getRecsByField f x =
  getRecsByField' f (\ _ -> return ()) x

getRecByField' :: ( DBAll val typ backend
                  , MonadIO m
                 )
               => EntityField val typ
               -> (SqlExpr (Entity val) -> SqlQuery a)
               -> typ
               -> ReaderT backend m (Maybe (Entity val))
getRecByField' f w x =
  selectFirst $
  from $ \ r -> do
    where_ (r ^. f ==. val x)
    void (w r)
    return r

getRecByField :: ( DBAll val typ backend
                 , MonadIO m
                 )
              => EntityField val typ
              -> typ
              -> ReaderT backend m (Maybe (Entity val))
getRecByField f x =
  getRecByField' f (\ _ -> return ()) x

-- data BlindLevel =
--     OwnerOnly
--   | AdminOnly
--   | EditorOnly

-- data Blinded l a =
--   Blinded a

-- type BlindEmail = Blinded 'AdminOnly EmailAddress


-- CustomFormInputFilledTextInput
-- CustomFormInputFilledTextboxInput
-- CustomFormInputFilledDropdownInput

-- data FieldType =
--     TextInput
--   | TextboxInput
--   | Dropdown (NonEmpty Text)

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

getUserPassword :: Email
                -> DB (Maybe
                       ( Entity User
                       , Entity Password
                       )
                      )
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


getUserByEmail :: Email -> DB (Maybe (Entity User))
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

defaultCreateUser :: Email
                  -> IO User
defaultCreateUser userEmail = do
  t <- getCurrentTime
  let userCreatedAt = t
  return $ User{..}

createUser :: Email -> Text -> DB (Entity User)
createUser email pass = do
  newUser <- liftIO $ defaultCreateUser email
  userId <- insert newUser
  hash <- liftIO $ hashPassword pass
  _ <- insert (Password hash userId)
  return (Entity userId newUser)

createOwner :: UserId -> DB (Entity Owner)
createOwner userKey = do
  owner <- insertEntity $ Owner userKey
  return owner

createAccount :: Email -> Text -> DB (Entity User, Entity Owner, Entity Account)
createAccount email pass = do
  user <- createUser email pass
  owner <- createOwner (entityKey user)
  account <- insertEntity $ Account (entityKey owner)
  return (user, owner, account)

--------------------------------------------------------------------------------
-- Conferences
--------------------------------------------------------------------------------

createConferenceForAccount :: AccountId -> Text -> Text -> DB (Entity Conference)
createConferenceForAccount accountId confName confDesc = do
  insertEntity $ Conference accountId confName confDesc

getConferencesByAccount :: AccountId -> DB [Entity Conference]
getConferencesByAccount accId = getRecsByField ConferenceAccount accId

getConference :: ConferenceId -> DB (Maybe (Entity Conference))
getConference confId = getRecByField ConferenceId confId

-- | This function uses inner joins because there is a foreign key constraint on
-- conferences to reference an 'Account', and a foreign key in the accounts
-- table that must reference an owner; i.e. Conferences must have owners, if
-- they exist.
getOwnerForConference
  :: ConferenceId
  -> DB (Maybe (Entity Conference, Entity Owner))
getOwnerForConference confId =
  selectFirst $
    from $ \(conference `InnerJoin` account `InnerJoin` owner) -> do
      on (account ^. AccountOwner ==. owner ^. OwnerId)
      on (conference ^. ConferenceAccount ==. account ^. AccountId)
      where_ (conference ^. ConferenceId ==. (val confId))
      pure (conference, owner)

-- | Return whether or not the user is an admin of the conference
-- Warning: Does not check if user is the owner of the conference
isUserConferenceAdmin :: UserId -> DB Bool
isUserConferenceAdmin userId = do
  mAdmin <- selectFirst $ from $ \admin ->
    where_ (admin ^. AdminUser ==. val (userId))
  case mAdmin of
    Nothing -> pure False
    Just _  -> pure True

--------------------------------------------------------------------------------
-- Abstracts
--------------------------------------------------------------------------------

getAbstractTypes :: ConferenceId -> DB [Entity AbstractType]
getAbstractTypes conferenceId =
  getRecsByField AbstractTypeConference conferenceId

getAbstractsForConference :: ConferenceId -> DB [(Entity Abstract, Entity AbstractType)]
getAbstractsForConference conferenceId =
  select $
    from $ \(abstractType `InnerJoin` abstract) -> do
      on (abstractType ^. AbstractTypeId ==. abstract ^. AbstractAbstractType)
      where_ (abstractType ^. AbstractTypeConference ==. val conferenceId)
      pure (abstract, abstractType)
