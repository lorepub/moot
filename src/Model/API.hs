module Model.API
  ( module Model.API
  , module Export
  ) where

import ClassyPrelude.Yesod hiding ((=.), (==.), hash, on, selectFirst, update)

import Database.Esqueleto hiding (selectFirst)
import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql
import Database.Esqueleto.Internal.Sql as Export (SqlQuery)

import Model as Export

import Data.Time.Clock
import Data.UUID (toByteString)
import Data.UUID.V4 (nextRandom)
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Database.Persist as P

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
                  -> Text
                  -> IO User
defaultCreateUser userEmail userName = do
  t <- getCurrentTime
  let userCreatedAt = t
  return $ User{..}

createUser :: Email -> Text -> Text -> DB (Entity User)
createUser email name pass = do
  newUser <- liftIO $ defaultCreateUser email name
  userId <- insert newUser
  hash <- liftIO $ hashPassword pass
  _ <- insert (Password hash userId)
  return (Entity userId newUser)

getUserByResetToken :: Token -> DB (Maybe (Entity User))
getUserByResetToken token =
  selectFirst $
  from $ \(r, u) -> do
  where_ (r ^. ResetUser ==. u ^. UserId &&. r ^. ResetToken ==. val token)
  return u

getUserPasswordByResetToken :: Token -> DB (Maybe (Entity User, Entity Password))
getUserPasswordByResetToken token =
  selectFirst $
  from $ \(r, u, p) -> do
  where_ (r ^. ResetUser ==. u ^. UserId &&. p ^. PasswordUser ==. u ^. UserId &&. r ^. ResetToken ==. val token)
  return (u, p)

resetUserPassword :: Token -> Text -> DB ()
resetUserPassword token newPassword = do
  (Just (_, Entity passwordKey _)) <- getUserPasswordByResetToken token
  newPasswordHash <- liftIO $ hashPassword newPassword
  P.update passwordKey [PasswordHash P.=. newPasswordHash]
  P.deleteBy $ UniqueToken token

createReset :: UserId -> DB (Entity Reset)
createReset userKey = do
  time  <- liftIO getCurrentTime
  token <- liftIO $ decodeUtf8 . B64.encode . toStrict . toByteString <$> nextRandom 
  reset <- insertEntity $ Reset (Token token) time userKey
  return reset

deleteOldResets :: DB ()
deleteOldResets = do
  oneDayAgo <- liftIO $ addUTCTime (negate nominalDay) <$> getCurrentTime
  deleteWhere [ResetCreatedAt P.<. oneDayAgo]

deleteExistingResets :: UserId -> DB ()
deleteExistingResets userId = do
  deleteWhere [ResetUser P.==. userId]

createOwner :: UserId -> DB (Entity Owner)
createOwner userKey = do
  owner <- insertEntity $ Owner userKey
  return owner

createAccount :: Email
              -> Text
              -> Text
              -> DB (Entity User, Entity Owner, Entity Account)
createAccount email name pass = do
  user <- createUser email name pass
  owner <- createOwner (entityKey user)
  account <- insertEntity $ Account (entityKey owner)
  return (user, owner, account)

--------------------------------------------------------------------------------
-- Conferences
--------------------------------------------------------------------------------

createConferenceForAccount :: AccountId
                           -> Text
                           -> Text
                           -> Markdown
                           -> Maybe UTCTime
                           -> Maybe UTCTime
                           -> DB (Entity Conference)
createConferenceForAccount accountId confName confDesc
  cfpIntro openingTime closingTime = do
  insertEntity
    $ Conference accountId confName confDesc
      cfpIntro openingTime closingTime

getConferencesByAccount :: AccountId -> DB [Entity Conference]
getConferencesByAccount accId = getRecsByField ConferenceAccount accId

getConferencesBySubmissions :: UserId
                            -> DB [( Entity Conference
                                   , Entity AbstractType
                                   , Entity Abstract
                                   )
                                  ]
getConferencesBySubmissions userId =
  select $
    from $ \(conference `InnerJoin` abstractType `InnerJoin` abstract) -> do
      on (abstractType ^. AbstractTypeId ==. abstract ^. AbstractAbstractType)
      on (conference ^. ConferenceId ==. abstractType ^. AbstractTypeConference)
      where_ (abstract ^. AbstractUser ==. val userId)
      pure (conference, abstractType, abstract)

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

getAbstractsForConference :: ConferenceId
                          -> DB [(Entity Abstract, Entity AbstractType)]
getAbstractsForConference conferenceId =
  select $ getAbstractsForConference' conferenceId

getAbstractsForConference' :: ( FromPreprocess
                                query expr backend (expr (Entity AbstractType))
                              , FromPreprocess
                                query expr backend (expr (Entity Abstract)))
                           => ConferenceId
                           -> query ( expr (Entity Abstract)
                                    , expr (Entity AbstractType))
getAbstractsForConference' conferenceId =
  -- Only get unblocked abstracts by default
  getAbstractsForConference'' (\_ _ -> return ()) False conferenceId

getAbstractsForConference'' :: ( FromPreprocess
                                 query expr backend (expr (Entity AbstractType))
                               , FromPreprocess
                                 query expr backend (expr (Entity Abstract)))
                            => ( expr (Entity AbstractType)
                              -> expr (Entity Abstract)
                              -> query ()
                               )
                            -> Bool
                            -> ConferenceId
                            -> query ( expr (Entity Abstract)
                                     , expr (Entity AbstractType))
getAbstractsForConference'' constraints blocked conferenceId =
  from $ \(abstractType `InnerJoin` abstract) -> do
    on (abstractType ^. AbstractTypeId ==. abstract ^. AbstractAbstractType)
    where_ (abstractType ^. AbstractTypeConference ==. val conferenceId)
    constraints abstractType abstract
    where_ (abstract ^. AbstractBlocked ==. val blocked)
    pure (abstract, abstractType)

updateAbstract :: AbstractId -> Text -> Markdown -> DB ()
updateAbstract abstractId title body = do
  update $ \a -> do
     set a [ AbstractEditedTitle =. val (Just title)
           , AbstractEditedAbstract =. val (Just body)
           ]
     where_ (a ^. AbstractId ==. val abstractId)

blockAbstract :: AbstractId -> DB ()
blockAbstract abstractId =
  update $ \a -> do
     set a [ AbstractBlocked =. val True
           ]
     where_ (a ^. AbstractId ==. val abstractId)

unblockAbstract :: AbstractId -> DB ()
unblockAbstract abstractId =
  update $ \a -> do
     set a [ AbstractBlocked =. val False
           ]
     where_ (a ^. AbstractId ==. val abstractId)

openConferenceCfp :: ConferenceId -> DB ()
openConferenceCfp confId = do
  t <- getDBTime
  update $ \c -> do
     set c [ ConferenceOpened =. val (Just t)
           , ConferenceClosed =. val Nothing
           ]
     where_ (c ^. ConferenceId ==. val confId)

closeConferenceCfp :: ConferenceId -> DB ()
closeConferenceCfp confId = do
  t <- getDBTime
  update $ \c -> do
     set c [ ConferenceClosed =. val (Just t)
           ]
     where_ (c ^. ConferenceId ==. val confId)

getDBTime :: DB UTCTime
getDBTime = do
  [Single r] <- rawSql "select now()" []
  return r
