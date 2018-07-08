{-# LANGUAGE StrictData #-}

module Model.Fixtures where

import Import

import Text.Email.QuasiQuotation

data UserFixtures =
  UserFixtures { allUsersF :: [Entity User] }
  deriving (Eq, Show)

data OwnerFixtures =
  OwnerFixtures { allOwnersF :: [Entity Owner] }
  deriving (Eq, Show)

data AccountFixtures =
  AccountFixtures { allAccountsF :: [Entity Account] }
  deriving (Eq, Show)

data ConferenceFixtures =
  ConferenceFixtures { allConferencesF :: [Entity Conference] }
  deriving (Eq, Show)

data Fixtures =
  Fixtures { userF :: UserFixtures
           , ownerF :: OwnerFixtures
           , accountF :: AccountFixtures
           , conferenceF :: ConferenceFixtures
           }
  deriving (Eq, Show)

chrisEmail :: Email
chrisEmail = [email|chris@lol.com|]
chrisPassword :: Text
chrisPassword = "chrisPass"

alexeyEmail :: Email
alexeyEmail = [email|alexey@lol.com|]
alexeyPassword :: Text
alexeyPassword = "alexeyPass"

makeAccount :: Email -> Text -> DB (Entity User, Entity Owner, Entity Account)
makeAccount email' pass = do
  createAccount email' pass

makeAccounts :: DB ([Entity User], [Entity Owner], [Entity Account])
makeAccounts =
  unzip3 <$>
  sequenceA [ makeAccount chrisEmail chrisPassword
            , makeAccount alexeyEmail alexeyPassword ]

{-# INLINABLE unsafeIdx #-}
unsafeIdx :: (MonoFoldable c) => c -> Integer -> Element c
unsafeIdx xs n
  | n < 0     = error "negative index"
  | otherwise =
    foldr (\x r k -> case k of
                       0 -> x
                       _ -> r (k-1)) (error ("index too large: " ++ show n))  xs n

makeConference :: AccountId -> Text -> DB (Entity Conference)
makeConference accountId confName =
  createConferenceForAccount accountId confName "The coolest code conf"

insertFixtures :: DB Fixtures
insertFixtures = do
  (allUsersF, allOwnersF, allAccountsF) <- makeAccounts
  -- let chris = unsafeIdx allUsersF 0
  --     alexey = unsafeIdx allUsersF 1
  let chrisAccount = unsafeIdx allAccountsF 0
  allConferencesF <-
    traverse (makeConference (entityKey chrisAccount)) ["ChrisConf"]
  let userF = UserFixtures {..}
      ownerF = OwnerFixtures {..}
      accountF = AccountFixtures {..}
      conferenceF = ConferenceFixtures {..}
  return Fixtures {..}


getTables :: DB [Text]
getTables = do
  tables <- rawSql [st|
                      SELECT table_name
                      FROM information_schema.tables
                      WHERE table_schema = 'public'
                      AND table_type='BASE TABLE';
                   |] []
  return $ map unSingle tables

truncateAllTables :: DB ()
truncateAllTables = do
  tables <- getTables
  sqlBackend <- ask
  let escapedTables :: [Text]
      escapedTables =
        map (connEscapeName sqlBackend . DBName) tables
      query =
        [st|TRUNCATE TABLE #{intercalate ", " escapedTables} RESTART IDENTITY CASCADE|]
  case escapedTables of
    [] ->
      error "List of tables is empty, something has gone wrong!"
    _ -> rawExecute query []

wipeAndReinstallFixtures :: DB ()
wipeAndReinstallFixtures = do
  truncateAllTables
  void $ insertFixtures

wipeAndMigrateDatabase :: DB ()
wipeAndMigrateDatabase = do
  truncateAllTables
  runMigrations
  void $ insertFixtures

freshDatabase :: DB ()
freshDatabase = do
  runMigrations
  void $ insertFixtures
