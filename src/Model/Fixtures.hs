{-# LANGUAGE StrictData #-}

module Model.Fixtures where

import Import

data UserFixtures =
  UserFixtures { allUsersF :: [Entity User] }
  deriving (Eq, Show)

data Fixtures =
  Fixtures { userF     :: UserFixtures
           }
  deriving (Eq, Show)

chrisEmail, chrisPassword :: Text
chrisEmail = "chris@lol.com"
chrisPassword = "chrisPass"

alexeyEmail, alexeyPassword :: Text
alexeyEmail = "alexey@lol.com"
alexeyPassword = "alexeyPass"

makeAccount :: Text -> Text -> DB (Entity User)
makeAccount email pass = do
  userEnt <- createUser email pass
  return userEnt

makeAccounts :: DB [Entity User]
makeAccounts =
  sequenceA [ makeAccount chrisEmail chrisPassword
            , makeAccount alexeyEmail alexeyPassword ]

{-# INLINABLE unsafeIdx #-}
unsafeIdx :: (MonoFoldable c) => c -> Integer -> Element c
unsafeIdx xs n
  | n < 0     = error "negative index"
  | otherwise = foldr (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) (error ("index too large: " ++ show n))  xs n

insertFixtures :: DB Fixtures
insertFixtures = do
  allUsersF <- makeAccounts
  -- let chris = unsafeIdx allUsersF 0
  --     alexey = unsafeIdx allUsersF 1
  let userF = UserFixtures {..}
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
