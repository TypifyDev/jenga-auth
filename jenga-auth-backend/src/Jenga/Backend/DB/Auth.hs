module Jenga.Backend.DB.Auth where

{-
Database queries around the Account, and UserType
tables
-}

import Jenga.Backend.Utils.HasTable
import Jenga.Backend.DB.Instances ()
import Jenga.Common.Auth
import Jenga.Common.Company
import Jenga.Common.Errors
import Jenga.Common.Schema
import Jenga.Common.OAuth
import Jenga.Common.BeamExtras

import Rhyolite.Account
import Rhyolite.Backend.Account
import Rhyolite.DB.Beam
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions
  (runInsertReturningList)
import qualified Database.Beam.Postgres.Full as PgFull
import Database.Beam.Backend.SQL.Types

import Data.Maybe
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Time.Clock


acctIDtoSerial :: Id Account -> SqlSerial Int64
acctIDtoSerial = Rhyolite.Account._accountId_id

acctIDtoInt64 :: Id Account -> Int64
acctIDtoInt64 = unSerial . acctIDtoSerial

getUsersEmail :: Database Postgres db => PgTable Postgres db Account -> Id Account -> Pg (Maybe T.Text)
getUsersEmail acctTbl aid = runSelectReturningOne $ select $ do
  accts <- all_ acctTbl
  guard_ $ _account_id accts ==. (val_ $ acctIDtoSerial aid)
  pure $ _account_email accts

-- getAccountByEmail :: T.Text -> Pg (Maybe (Account Identity))
-- getAccountByEmail email = getUserByEmail email

getUserByEmail, getAccountByEmail
  :: Database Postgres db
  => PgTable Postgres db Account
  -> T.Text
  -> Pg (Maybe (Account Identity))
getAccountByEmail = getUserByEmail
getUserByEmail acctTbl email = runSelectReturningOne $ select $ do
  accts <- all_ acctTbl
  guard_ $ _account_email accts ==. (lower_ (val_ email))
  pure accts


-- | Look up a user by email, verifying they belong to the same company as the admin
getUserByEmailInOrg
  :: Database Postgres db
  => PgTable Postgres db Account
  -> PgTable Postgres db UserTypeTable
  -> PgTable Postgres db OrgOwnedUsers
  -> Id Account
  -> T.Text
  -> Pg (Maybe (Account Identity))
getUserByEmailInOrg acctTbl uTypeTbl orgUsersTbl aid email = runSelectReturningOne $ select $ do
  -- Get admin's company
  uType <- filter_ (\u -> _userType_acctID u ==. (val_ $ acctIDtoInt64 aid)) $ all_ uTypeTbl
  let CompanyInfoId mCompanyId = _userType_companyID uType
  -- Find target account by email
  targetAcct <- filter_ (\a -> _account_email a ==. val_ email) $ all_ acctTbl
  -- Verify target is owned by admin's company
  orgUser <- filter_ (\ou ->
        _orgOwnedUsers_accountId ou ==. primaryKey targetAcct
    ) $ all_ orgUsersTbl
  let CompanyInfoId ouCompanyId = _orgOwnedUsers_companyId orgUser
  guard_ $ just_ ouCompanyId ==. mCompanyId
  pure targetAcct

-- | Nothing -> Self ; Just companyId -> Admin
putNewUserType :: Database Postgres db => PgTable Postgres db UserTypeTable -> Id Account -> Maybe (PrimaryKey CompanyInfo Identity) -> Pg ()
putNewUserType uTypeTbl (AccountId (SqlSerial i)) = \case
  Just (CompanyInfoId cid) -> runInsert $ insert uTypeTbl $ insertExpressions
    [ UserTypeTable (val_ i) (val_ Admin) (CompanyInfoId $ val_ $ Just cid) ]
  Nothing -> runInsert $ insert uTypeTbl $ insertExpressions
    [ UserTypeTable (val_ i) (val_ Self) (CompanyInfoId $ val_ Nothing) ]

getUserType :: Database Postgres db => PgTable Postgres db UserTypeTable -> Id Account -> Pg (Maybe UserType)
getUserType uTypeTbl acctId = do
  mUserType <- runSelectReturningOne $ select $ do
    userTypes <- all_ uTypeTbl
    guard_ $ _userType_acctID userTypes ==. (val_ $ acctIDtoInt64 acctId)
    pure $ userTypes
  pure $ _userType_userType <$> mUserType

-- | is this Used??
selectAccountData
  :: Database Postgres db
  => PgTable Postgres db Account
  -> PrimaryKey Account Identity
  -> Pg (Maybe (Account Identity))
selectAccountData acctsTbl accountID =
  runSelectReturningOne $ (lookup_ acctsTbl accountID)

resetPassword'
  :: (Database Postgres db)
  => DatabaseEntity Postgres db (TableEntity Account)
  -> Id Account
  -> UTCTime
  -> T.Text
  -> Pg (Either (BackendError ResetPasswordError) (PrimaryKey Account Identity))
resetPassword' tbl aid t pw = do
  hash <- makePasswordHash pw
  resetPasswordHash' tbl aid t hash

resetPasswordHash'
  :: (Database Postgres db)
  => DatabaseEntity Postgres db (TableEntity Account)
  -> Id Account
  -> UTCTime
  -> BS.ByteString
  -> Pg (Either (BackendError ResetPasswordError) (PrimaryKey Account Identity))
resetPasswordHash' accountTable aid nonce pwhash = do
  macc <- runSelectReturningOne $ lookup_ accountTable aid
  case macc of
    Nothing -> return $ Left $ BCritical CouldntRetrieveAccount
    Just a ->
      if _account_passwordResetNonce a == Just nonce
      then do
        setAccountPasswordHash accountTable aid pwhash
        return $ Right aid
      else do
        return $ Left $ BUserError InvalidToken

isEmptyPassword :: Database Postgres db => PgTable Postgres db Account -> Id Account -> Pg Bool
isEmptyPassword acctTbl accountID = f >>= \case
  Just True -> pure True
  _ -> pure False
  where
    f = runSelectReturningOne $ select $ do
      account <- all_ acctTbl
      guard_ $ _account_id account ==. (val_ $ Rhyolite.Account._accountId_id accountID)
      return $ isNothing_ $ _account_password account

doesAccountExist :: (Database Postgres db)
                 => PgTable Postgres db Account
                 -> T.Text
                 -> Pg (Maybe (PrimaryKey Account Identity))
doesAccountExist accountTable email = do
  runSelectReturningOne $ select $ fmap primaryKey $ filter_ (\x ->
    lower_ (_account_email x) ==. lower_ (val_ email)) $ all_ accountTable

data EnsureAccountError
  = EnsureAccount_InsertReturnedUnexpectedRows Int
  deriving (Eq, Show)

-- | Only diff is no notification
ensureAccountExists'
  :: (Database Postgres db)
  => PgTable Postgres db Account
  -> T.Text
  -> Pg (Either EnsureAccountError (Bool, Id Account))
ensureAccountExists' accountTable email = do
  existingAccountId <- runSelectReturningOne $ select $ fmap primaryKey $ filter_ (\x ->
    lower_ (_account_email x) ==. lower_ (val_ email)) $ all_ accountTable
  case existingAccountId of
    Just existing -> return $ Right (False, existing)
    Nothing -> do
      results <- runInsertReturningList $ insert accountTable $ insertExpressions
        [ Account
            { _account_id = default_
            , _account_email = lower_ (val_ email)
            , _account_password = nothing_
            , _account_passwordResetNonce = nothing_
            }
        ]
      case results of
        [acc] -> do
          let aid = primaryKey acc
          pure $ Right (True, aid)
        other -> pure $ Left $ EnsureAccount_InsertReturnedUnexpectedRows (length other)

-- | This only exists cuz we'd rather ensure the SideEffect of creating a subscription works first and want to
-- | avoid cases where a server fault happens despite a charge being created
deleteFailedAccount :: Database Postgres db => PgTable Postgres db Account -> T.Text -> Pg ()
deleteFailedAccount = deleteUserByEmail

-- | I question if we need to review and if we create func: deleteUserById
-- | that this plugs into
-- |
-- | Note: we should have one of these that is a HOF, and says how to clean up
-- | domain-specific non-critical data (everything else, we dont control)
deleteUserByEmail :: Database Postgres db => PgTable Postgres db Account -> T.Text -> Pg ()
deleteUserByEmail acctTbl email = runDelete $ delete acctTbl $ (\acct -> _account_email acct ==. (val_ email))


-- newAccount :: PgTable Postgres db Account -> NewAccount -> Pg (Maybe (PrimaryKey Account Identity))
-- newAccount (NewAccount email mPass) = do
--   fmap listToMaybe $ PgFull.runPgInsertReturningList $ flip PgFull.returning primaryKey $ insert (_db_accounts db)
--     $ insertExpressions  [ Account default_ (val_ email) (val_ mPass) (just_ current_timestamp_) ]


newAccount
  :: Database Postgres db
  => PgTable Postgres db Account
  -> Email
  -> Maybe Password
  -> Pg (Maybe (Id Account))
newAccount acctTbl (Email email) mPass = do
  case mPass of
    Nothing -> do
      fmap listToMaybe $ PgFull.runPgInsertReturningList $ flip PgFull.returning primaryKey $ insert acctTbl
        $ insertExpressions  [ Account default_ (val_ email) (val_ Nothing) (just_ current_timestamp_) ]
    Just (Password pass) -> do
      hashed <- makePasswordHash pass
      fmap listToMaybe $ PgFull.runPgInsertReturningList $ flip PgFull.returning primaryKey $ insert acctTbl
        $ insertExpressions  [ Account default_ (val_ email) (val_ $ Just hashed) (just_ current_timestamp_) ]

-- | TODO: this would be an interesting try: how can we pass a custom
--   way to get a name?
--
-- getUserAsAddress :: Id Account -> Pg (Maybe Address)
-- getUserAsAddress aid = do
--   mA <- runSelectReturningOne $ select $ do
--     accts <- all_ (_db_accounts db)
--     guard_ $ primaryKey accts ==. (val_ aid)
--     uInfos <- join_ (_db_userInfo db) $ \uInfo -> (AccountId $ _userInfo_uid uInfo) ==. primaryKey accts
--     pure (_account_email accts, _userInfo_fullName uInfos)
--   pure $ (\(e,n) -> Address (Just n) e) <$> mA


insertNewGithubID
  :: PgTable Postgres db GithubID
  -> GitHubUser
  -> PrimaryKey Account Identity
  -> Pg ()
insertNewGithubID githubOAuthTbl githubUser (AccountId uid) = do
  runInsert $ insert githubOAuthTbl $ insertExpressions
    [ GithubID
      (val_ $ fromIntegral $ _githubUser_github_id githubUser)
      (val_ $ unSerial uid)
    ]

getGithubUserIfTheyExist
  :: Database Postgres db
  => PgTable Postgres db GithubID
  -> GitHubUser
  -> Pg (Maybe (GithubID Identity))
getGithubUserIfTheyExist githubOAuthTbl ghUser = runSelectReturningOne $ select $ do
  gits <- all_ githubOAuthTbl
  guard_ $ (_githubID_ghid gits) ==. (val_ $ fromIntegral $ _githubUser_github_id ghUser)
  pure gits

insertNewGoogleID
  :: PgTable Postgres db GoogleID
  -> GoogleUser
  -> PrimaryKey Account Identity
  -> Pg ()
insertNewGoogleID googleOAuthTbl googleUser (AccountId uid) = do
  runInsert $ insert googleOAuthTbl $ insertExpressions
    [ GoogleID
      (val_ $ T.pack $ _googleUser_id googleUser)
      (val_ $ unSerial uid)
    ]

getGoogleUserIfTheyExist
  :: Database Postgres db
  => PgTable Postgres db GoogleID
  -> GoogleUser
  -> Pg (Maybe (GoogleID Identity))
getGoogleUserIfTheyExist googleOAuthTbl googleUser = runSelectReturningOne $ select $ do
  googleIds <- all_ googleOAuthTbl
  guard_ $ (_googleID_gid googleIds) ==. (val_ $ T.pack $ _googleUser_id googleUser)
  pure googleIds

insertNewDiscordID
  :: PgTable Postgres db DiscordID
  -> DiscordUser
  -> PrimaryKey Account Identity
  -> Pg ()
insertNewDiscordID discordOAuthTbl discordUser (AccountId uid) = do
  runInsert $ insert discordOAuthTbl $ insertExpressions
    [ DiscordID
      (val_ $ T.pack $ _discordUser_id discordUser)
      (val_ $ unSerial uid)
    ]

getDiscordUserIfTheyExist
  :: Database Postgres db
  => PgTable Postgres db DiscordID
  -> DiscordUser
  -> Pg (Maybe (DiscordID Identity))
getDiscordUserIfTheyExist discordOAuthTbl discordUser = runSelectReturningOne $ select $ do
  discordIds <- all_ discordOAuthTbl
  guard_ $ (_discordID_did discordIds) ==. (val_ $ T.pack $ _discordUser_id discordUser)
  pure discordIds
