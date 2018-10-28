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
import Data.UUID (UUID, toByteString)
import Data.UUID.V4 (nextRandom)
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Database.Persist as P
import qualified Database.Esqueleto.Internal.Language as Esq

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
      userVerifiedAt = Nothing
  return $ User{..}

createUser :: Email -> Text -> Text -> DB (Entity User, Entity EmailVerification)
createUser email name pass = do
  newUser <- liftIO $ defaultCreateUser email name
  userId <- insert newUser
  hash <- liftIO $ hashPassword pass
  _ <- insert (Password hash userId)
  emailVerification <- createEmailVerification userId
  return (Entity userId newUser, emailVerification)

createEmailVerification :: UserId -> DB (Entity EmailVerification)
createEmailVerification userId = do
  uuid <- liftIO nextRandom
  t <- liftIO getCurrentTime
  insertEntity $ EmailVerification uuid t userId

verifyEmail :: UUID -> DB (Maybe ())
verifyEmail uuid = do
  emailVerificationM <- getRecByField EmailVerificationUuid uuid
  case emailVerificationM of
    Nothing -> do
      -- ????
      return Nothing
    (Just ev) -> do
      setUserVerified ev
      P.delete (entityKey ev)
      return (Just ())
  where setUserVerified :: Entity EmailVerification -> DB ()
        setUserVerified (Entity _ ev) = do
          t <- liftIO getCurrentTime
          P.update (emailVerificationUser ev) [UserVerifiedAt P.=. Just t]

getUserByResetToken :: Token -> DB (Maybe (Entity User))
getUserByResetToken token =
  selectFirst $
  from $ \(r, u) -> do
  where_ (r ^. ResetUser ==. u ^. UserId)
  where_ (r ^. ResetToken ==. val token)
  return u

getUserPasswordByResetToken :: Token -> DB (Maybe (Entity User, Entity Password))
getUserPasswordByResetToken token =
  selectFirst $
  from $ \(r, u, p) -> do
  where_ (r ^. ResetUser ==. u ^. UserId)
  where_ (r ^. ResetToken ==. val token)
  where_ (p ^. PasswordUser ==. u ^. UserId)
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
              -> DB ( Entity User
                    , Entity EmailVerification
                    , Entity Owner
                    , Entity Account
                    )
createAccount email name pass = do
  (user, emailVerification) <- createUser email name pass
  owner <- createOwner (entityKey user)
  account <- insertEntity $ Account (entityKey owner)
  return (user, emailVerification, owner, account)

--------------------------------------------------------------------------------
-- Conferences
--------------------------------------------------------------------------------

createConferenceForAccount :: AccountId
                           -> Text
                           -> Text
                           -> ConferenceSlug
                           -> Markdown
                           -> Maybe UTCTime
                           -> Maybe UTCTime
                           -> DB (Entity Conference)
createConferenceForAccount accountId confName confDesc confCode
  cfpIntro openingTime closingTime = do
  conf <- insertEntity
    $ Conference accountId confName confDesc
      cfpIntro openingTime closingTime
  _ <- createSlug True (entityKey conf) confCode
  pure conf

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

getAbstractTypeByConferenceAndId :: ConferenceId -> AbstractTypeId -> DB (Maybe (Entity AbstractType))
getAbstractTypeByConferenceAndId confId atId =
  selectFirst $
    from $ \(abstractType) -> do
      where_ (abstractType ^. AbstractTypeConference ==. val confId)
      where_ (abstractType ^. AbstractTypeId ==. val atId)
      pure abstractType

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

getAbstractsAndAuthorsForConference''
  :: ( FromPreprocess
       query expr backend (expr (Entity AbstractType))
     , FromPreprocess
       query expr backend (expr (Entity Abstract))
     , FromPreprocess
       query expr backend (expr (Entity User))
     )
  => ( expr (Entity AbstractType)
    -> expr (Entity Abstract)
    -> expr (Entity User)
    -> query ()
     )
  -> Bool
  -> ConferenceId
  -> query ( expr (Entity Abstract)
           , expr (Entity User)
           , expr (Entity AbstractType))
getAbstractsAndAuthorsForConference'' constraints blocked conferenceId =
   getAbstractsAndAuthorsForConference''' constraints blocked conferenceId Nothing (,,)

getAbstractsAndAuthorsForConferenceCnt
  :: ( FromPreprocess
       query expr backend (expr (Entity AbstractType))
     , FromPreprocess
       query expr backend (expr (Entity Abstract))
     , FromPreprocess
       query expr backend (expr (Entity User))
     , Esqueleto query expr2 backend
     )
  => ( expr (Entity AbstractType)
    -> expr (Entity Abstract)
    -> expr (Entity User)
    -> query ()
     )
  -> Bool
  -> ConferenceId
  -> query (expr2 (Esq.Value Int64))
getAbstractsAndAuthorsForConferenceCnt constraints blocked conferenceId =
   getAbstractsAndAuthorsForConference''' constraints blocked conferenceId Nothing (\_ _ _ -> Esq.countRows)

getAbstractsAndAuthorsForConferencePage
  :: ( FromPreprocess
       query expr backend (expr (Entity AbstractType))
     , FromPreprocess
       query expr backend (expr (Entity Abstract))
     , FromPreprocess
       query expr backend (expr (Entity User))
     )
  => ( expr (Entity AbstractType)
    -> expr (Entity Abstract)
    -> expr (Entity User)
    -> query ()
     )
  -> Bool
  -> ConferenceId
  -> OffsetAndLimit
  -> query ( expr (Entity Abstract)
           , expr (Entity User)
           , expr (Entity AbstractType))
getAbstractsAndAuthorsForConferencePage constraints blocked conferenceId offsetAndLimit =
   getAbstractsAndAuthorsForConference''' constraints blocked conferenceId (Just offsetAndLimit) (,,)

data OffsetAndLimit = OffsetAndLimit Int64 Int64

getAbstractsAndAuthorsForConference'''
  :: (FromPreprocess query expr backend (expr (Entity AbstractType)),
      FromPreprocess query expr backend (expr (Entity Abstract)),
      FromPreprocess query expr backend (expr (Entity User))) =>
     (expr (Entity AbstractType)
       -> expr (Entity Abstract)
       -> expr (Entity User)
       -> query a)
     -> Bool
     -> Key Conference
     -> Maybe OffsetAndLimit
     -> (expr (Entity Abstract) -> expr (Entity User) -> expr (Entity AbstractType) -> b)
     -> query b
getAbstractsAndAuthorsForConference''' constraints blocked conferenceId offsetAndLimit resultF =
  from $ \(abstractType `InnerJoin` abstract `InnerJoin` user) -> do
    on (user ^. UserId ==. abstract ^. AbstractUser)
    on (abstractType ^. AbstractTypeId ==. abstract ^. AbstractAbstractType)
    where_ (abstractType ^. AbstractTypeConference ==. val conferenceId)
    _ <- constraints abstractType abstract user
    where_ (abstract ^. AbstractBlocked ==. val blocked)
    _ <- case offsetAndLimit of
        Nothing -> pure ()
        Just (OffsetAndLimit off lim) -> do
               offset off
               limit lim
    pure (resultF abstract user abstractType)

updateAbstractType :: AbstractTypeId -> Text -> Word64 -> DB ()
updateAbstractType abstractTypeId name duration =
  update $ \a -> do
     set a [ AbstractTypeName =. val name
           , AbstractTypeDuration =. val (makeTalkDuration duration)
           ]
     where_ (a ^. AbstractTypeId ==. val abstractTypeId)

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

--------------------------------------------------------------------------------
-- Slugs
--------------------------------------------------------------------------------

data SlugErr =
    SlugMissing
    -- ^ indicates missing code
  | SlugInactive ConferenceSlug
    -- ^ indicates use of old inactive code, active code is returned
  deriving Show

-- | Database decoding of slugs
--   decodes 'ConferenceSlug' into 'ConferenceId'
--  'SlugErr' is used if 'ConferenceSlug' is invalid or not active.
resolveConferenceId :: ConferenceSlug -> DB (Either SlugErr ConferenceId)
resolveConferenceId conferenceCode = do
     maybeSlugE <- selectFirst $
        from $ \slug -> do
          where_ (slug ^. SlugCode ==. val conferenceCode)
          pure slug
     case maybeSlugE of
        Nothing -> pure . Left $ SlugMissing
        Just slug ->
          if slugActive . entityVal $ slug
          then pure . Right . slugConference . entityVal $ slug
          else do
            activeCode <- getActiveConferenceSlug . slugConference $ entityVal slug
            pure . maybe (Left $ SlugMissing) (Left . SlugInactive) $ activeCode

-- | User invoked slug creation and activation.
--   It deactives previous active code and activates the new code
--   only one code is active per conference if this API is used
createOrActivateSlug :: ConferenceId
                    -> ConferenceSlug  -- ^ new (to be created and activated) or old (to be just activated) code
                    -> DB (ConferenceSlug)
createOrActivateSlug conferenceId conferenceCode = do
      maybeCurrent <- getActiveConferenceSlug conferenceId
      useSlugE <- case maybeCurrent of
         Nothing ->
           -- This conference does not have any slugs
           -- (otherwise there should be always one active)
           createSlug True conferenceId conferenceCode
         Just currentCode -> do
           -- | current slug exists, deactivate
           update $ \s -> do
             set s [ SlugActive =. val False]
             where_ (s ^. SlugCode ==. val currentCode)
           -- | are we reactivating old slug?
           maybeMatching <- resolveConferenceIdStrict conferenceId conferenceCode
           case maybeMatching of
              Nothing -> createSlug True conferenceId conferenceCode
              Just matching -> do
                  -- | matching found just activate
                  update $ \s -> do
                     set s [ SlugActive =. val True
                           ]
                     where_ (s ^. SlugCode ==. val conferenceCode)
                  pure matching
      pure $ slugCode . entityVal $ useSlugE

-- | convenient to use in fixtures, it is safe to create inactive slugs
addInactiveConferenceSlug :: ConferenceId -> ConferenceSlug -> DB (Entity Slug)
addInactiveConferenceSlug = createSlug False

--------------------------------------------------------------------------------
-- Slugs private primitives
--------------------------------------------------------------------------------
getActiveConferenceSlug :: ConferenceId -> DB (Maybe ConferenceSlug)
getActiveConferenceSlug conferenceId = do
       mslug <- selectFirst $
          from $ \slug -> do
           where_ ((slug ^. SlugConference ==. val conferenceId) &&. (slug ^. SlugActive ==. val True))
           pure slug
       pure . fmap (slugCode . entityVal) $ mslug


-- | use createOrActivateSlug instead unless this is first slug for the conference
createSlug :: Bool -> ConferenceId -> ConferenceSlug -> DB (Entity Slug)
createSlug active conferenceId conferenceCode = do
    insertEntity
       $ Slug conferenceId conferenceCode active

resolveConferenceIdStrict :: ConferenceId -> ConferenceSlug -> DB (Maybe (Entity Slug))
resolveConferenceIdStrict conferenceId conferenceCode = selectFirst $
   from $ \slug -> do
    where_ ((slug ^. SlugConference ==. val conferenceId) &&. (slug ^. SlugCode ==. val conferenceCode))
    pure slug

getConfAndAbstractTypes :: ConferenceId
                        -> DB (Maybe (Entity Conference, [Entity AbstractType]))
getConfAndAbstractTypes confId = do
  maybeConf <- getConference confId
  case maybeConf of
    Nothing -> return Nothing
    (Just conf) -> do
      abstractTypes <- getAbstractTypes confId
      return $ Just $ (conf, abstractTypes)
