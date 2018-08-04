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

data AbstractFixtures =
  AbstractFixtures { allAbstractsF :: [Entity Abstract] }
  deriving (Eq, Show)

data AbstractTypeFixtures =
  AbstractTypeFixtures { allAbstractTypesF :: [Entity AbstractType] }
  deriving (Eq, Show)

data Fixtures =
  Fixtures { userF :: UserFixtures
           , ownerF :: OwnerFixtures
           , accountF :: AccountFixtures
           , conferenceF :: ConferenceFixtures
           , abstractTypeF :: AbstractTypeFixtures
           , abstractF :: AbstractFixtures
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

waddlestonEmail :: Email
waddlestonEmail = [email|waddleston@lol.com|]
waddlestonPassword :: Text
waddlestonPassword = "waddlesPass"

makeAccount :: Email -> Text -> DB (Entity User, Entity Owner, Entity Account)
makeAccount email' pass = do
  createAccount email' pass

makeAccounts :: DB ([Entity User], [Entity Owner], [Entity Account])
makeAccounts =
  unzip3 <$>
  sequenceA [ makeAccount chrisEmail chrisPassword
            , makeAccount alexeyEmail alexeyPassword
            ]

makeUser :: Email -> Text -> DB (Entity User)
makeUser email' pass = do
  createUser email' pass

makeUsers :: DB [Entity User]
makeUsers =
  sequenceA [ makeUser waddlestonEmail waddlestonPassword ]

{-# INLINABLE unsafeIdx #-}
unsafeIdx :: (MonoFoldable c) => c -> Integer -> Element c
unsafeIdx xs n
  | n < 0     = error "negative index"
  | otherwise =
    foldr (\x r k -> case k of
                       0 -> x
                       _ -> r (k-1)) (error ("index too large: " ++ show n))  xs n

makeConference :: AccountId
               -> Text
               -> DB (Entity Conference)
makeConference accountId confName =
  createConferenceForAccount accountId confName "The coolest code conf"

makeAbstractType :: ConferenceId
                 -> TalkDuration
                 -> Text
                 -> DB (Entity AbstractType)
makeAbstractType abstractTypeConference
                 abstractTypeDuration
                 abstractTypeName =
  insertEntity AbstractType{..}

markdownifiedAbstractBody :: Markdown
markdownifiedAbstractBody = Markdown [st|
# Bananas, Lenses and Barbreh Strysen

```haskell
conferenceAbstractView (Entity confId conference)
  (Entity abstractId Abstract{..}) widget enctype = do
  abstractMarkdown <- renderMarkdown abstractAuthorAbstract
  abstractEditedMarkdown <-
    traverse renderMarkdown abstractEditedAbstract
```

Edited abstract 2
|]

makeAbstract :: UserId
             -> AbstractTypeId
             -> Text
             -> DB (Entity Abstract)
makeAbstract abstractUser abstractAbstractType abstractAuthorTitle = do
  let abstractAuthorAbstract = markdownifiedAbstractBody
      abstractEditedTitle = Nothing
      abstractEditedAbstract = Nothing
  insertEntity Abstract{..}

insertFixtures :: DB Fixtures
insertFixtures = do
  (accountUsersF, allOwnersF, allAccountsF) <- makeAccounts
  plainUsersF <- makeUsers
  let allUsersF = accountUsersF <> plainUsersF
      chrisAccount = unsafeIdx allAccountsF 0
      chrisAccountK = entityKey chrisAccount
      alexeyAccount = unsafeIdx allAccountsF 1
      alexeyAccountK = entityKey alexeyAccount
      waddlesUser = unsafeIdx plainUsersF 0
      waddlesUserK = entityKey waddlesUser
  chrisConferencesF <-
    traverse (makeConference chrisAccountK)
      ["Chris Conf 9000", "Chris's Other Conf", "Chris's Best Conf" ]
  alexeyConferencesF <-
    traverse (makeConference alexeyAccountK)
      ["Alexey Conf 9000", "Alexey's Other Conf", "Alexeys's Best Conf" ]

  let chrisFirstConf = unsafeIdx chrisConferencesF 0
      chrisFirstConfK = entityKey chrisFirstConf
      shortTalkDur = TalkDuration (Minutes 15)
      defaultTalkDur = TalkDuration (Minutes 60)
      longTalkDur = TalkDuration (Minutes 120)
  allAbstractTypesF <-
    traverse (uncurry $ makeAbstractType chrisFirstConfK)
    [ ( defaultTalkDur, "Goldilocks" )
    , ( shortTalkDur, "Crisp" )
    , ( longTalkDur, "The Thing" )
    ]

  let goldilocksAbstractType = unsafeIdx allAbstractTypesF 0
      goldilocksAbstractTypeK = entityKey goldilocksAbstractType
      crispAbstractType = unsafeIdx allAbstractTypesF 1
      crispAbstractTypeK = entityKey crispAbstractType
      theThingAbstractType = unsafeIdx allAbstractTypesF 2
      theThingAbstractTypeK = entityKey theThingAbstractType

  allAbstractsF <-
    traverse (uncurry $ makeAbstract waddlesUserK)
      [ ( theThingAbstractTypeK
        , "zygohistomorphic prepromorphisms and their malcontents"
        )
      , ( crispAbstractTypeK
        , "crispy potato chips and their manifold destiny"
        )
      , ( goldilocksAbstractTypeK
        , "Bananas, Lenses and Barbreh Strysend"
        )
      ]

  let allConferencesF = chrisConferencesF ++ alexeyConferencesF
      userF = UserFixtures {..}
      ownerF = OwnerFixtures {..}
      accountF = AccountFixtures {..}
      conferenceF = ConferenceFixtures {..}
      abstractTypeF = AbstractTypeFixtures {..}
      abstractF = AbstractFixtures {..}
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

runMigrationsUnsafe :: DB ()
runMigrationsUnsafe = runMigrationUnsafe migrateAll

wipeAndReinstallFixtures :: DB ()
wipeAndReinstallFixtures = do
  truncateAllTables
  void $ insertFixtures

wipeAndMigrateDatabase :: DB ()
wipeAndMigrateDatabase = do
  truncateAllTables
  runMigrationsUnsafe
  void $ insertFixtures

freshDatabase :: DB ()
freshDatabase = do
  runMigrationsUnsafe
  void $ insertFixtures
