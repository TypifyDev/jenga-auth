{-# LANGUAGE ScopedTypeVariables #-}
module Jenga.Backend.DB.OrgBased where

import Jenga.Common.Schema
import Jenga.Common.Company
import Jenga.Common.Errors
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.DB.Auth
import Jenga.Common.BeamExtras
import Jenga.Common.Auth

import Rhyolite.Account
import Database.Beam.Postgres
import Database.Beam.Schema
import Database.Beam.Query

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Pool
import Data.Maybe (isJust)
import Data.Functor.Identity
import Data.Int (Int64)
import qualified Data.Text as T

isOrgOwnedUser
  :: Database Postgres db
  => PgTable Postgres db OrgOwnedUsers
  -> Id Account
  -> Pg Bool
isOrgOwnedUser tbl acctId = do
  result <- runSelectReturningOne $ select $ do
    row <- all_ tbl
    guard_ $ _orgOwnedUsers_accountId row ==. val_ acctId
    pure row
  pure $ isJust result

addOrgOwnedUser
  :: PgTable Postgres db OrgOwnedUsers
  -> Id Account
  -> PrimaryKey CompanyInfo Identity
  -> Pg ()
addOrgOwnedUser tbl acctId companyId = runInsert $ insert tbl $ insertExpressions
  [ OrgOwnedUsers (val_ acctId) (val_ companyId)
  ]

getThisUsersOrg
  :: Database Postgres db
  => PgTable Postgres db OrgOwnedUsers
  -> Id Account
  -> Pg (Maybe (OrgOwnedUsers Identity))
getThisUsersOrg tbl acctId = runSelectReturningOne $ select $ do
  row <- all_ tbl
  guard_ $ _orgOwnedUsers_accountId row ==. val_ acctId
  pure row

-- | Look up the company ID for the given account, then run the continuation.
-- Returns NoAuth if the account has no associated company (i.e. not an admin).
runAdmin
  :: forall db m cfg e a.
     ( Database Postgres db
     , MonadIO m
     , HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db UserTypeTable
     )
  => Id Account
  -> (PrimaryKey CompanyInfo Identity -> ReaderT cfg m (Either (BackendError e) a))
  -> ReaderT cfg m (Either (BackendError e) a)
runAdmin aid k = do
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  mCompanyId <- withDbEnv $ getCompanyId uTypeTbl aid
  case mCompanyId of
    Nothing -> pure $ Left NoAuth
    Just companyId -> k companyId

-- | Verify admin auth, get company ID and org user account IDs.
runWithMyUsers
  :: forall db m cfg e a.
     ( Database Postgres db
     , MonadIO m
     , HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     )
  => Id Account
  -> (PrimaryKey CompanyInfo Identity -> [Id Account] -> ReaderT cfg m (Either (BackendError e) a))
  -> ReaderT cfg m (Either (BackendError e) a)
runWithMyUsers acctID k =
  runAdmin @db acctID $ \cId -> do
    (orgTbl :: PgTable Postgres db OrgOwnedUsers) <- asksTableM
    orgUsers <- withDbEnv $ getOrgUsers orgTbl cId
    k cId (_orgOwnedUsers_accountId <$> orgUsers)

-- | Get the CompanyInfo ID for an admin account
getCompanyId
  :: Database Postgres db
  => PgTable Postgres db UserTypeTable
  -> Id Account
  -> Pg (Maybe (PrimaryKey CompanyInfo Identity))
getCompanyId uTypeTbl aid = do
  mUType <- runSelectReturningOne $ select $ do
    uTypes <- all_ uTypeTbl
    guard_ $ _userType_acctID uTypes ==. (val_ $ acctIDtoInt64 aid)
    pure uTypes
  pure $ do
    uType <- mUType
    let CompanyInfoId mCid = _userType_companyID uType
    CompanyInfoId <$> mCid

getOrgName
  :: Database Postgres db
  => PgTable Postgres db CompanyInfo
  -> PrimaryKey CompanyInfo Identity
  -> Pg (Maybe T.Text)
getOrgName companyTbl cid = do
  mCompany <- runSelectReturningOne $ lookup_ companyTbl cid
  pure $ _companyInfo_name <$> mCompany

getAdminOrgName
  :: Database Postgres db
  => PgTable Postgres db CompanyInfo
  -> PrimaryKey CompanyInfo Identity
  -> Pg (Maybe T.Text)
getAdminOrgName = getOrgName

getOrgUsers
  :: Database Postgres db
  => PgTable Postgres db OrgOwnedUsers
  -> PrimaryKey CompanyInfo Identity
  -> Pg [OrgOwnedUsers Identity]
getOrgUsers tbl companyId = runSelectReturningList $ select $ do
  row <- all_ tbl
  let CompanyInfoId cid = _orgOwnedUsers_companyId row
      CompanyInfoId targetCid = companyId
  guard_ $ cid ==. val_ targetCid
  pure row

putAccountRelations
  :: Database Postgres db
  => PgTable Postgres db UserTypeTable
  -> PgTable Postgres db OrgOwnedUsers
  -> PrimaryKey Account Identity
  -> IsUserType
  -> Pg (Either UserSignupError ())
putAccountRelations uTypeTbl orgTable aid = \case
  IsGroupUser _email companyId -> do
    tryPutGroupUser uTypeTbl orgTable aid companyId
  IsSelf -> do
    putNewUserType uTypeTbl aid Nothing
    pure $ Right ()
  IsCompany companyId -> do
    putNewUserType uTypeTbl aid (Just companyId)
    pure $ Right ()

tryPutGroupUser
  :: Database Postgres db
  => PgTable Postgres db UserTypeTable
  -> PgTable Postgres db OrgOwnedUsers
  -> PrimaryKey Account Identity
  -> PrimaryKey CompanyInfo Identity
  -> Pg (Either UserSignupError ())
tryPutGroupUser uTypeTbl orgTable aid companyId = do
  lookupCompanyInUserTypeTable uTypeTbl companyId >>= \case
    Nothing -> pure $ Left NoLinkedOrganization
    Just _ -> do
      putNewUserType uTypeTbl aid Nothing
      addOrgOwnedUser orgTable aid companyId
      pure $ Right ()

lookupCompanyInUserTypeTable
  :: Database Postgres db
  => PgTable Postgres db UserTypeTable
  -> PrimaryKey CompanyInfo Identity
  -> Pg (Maybe (UserTypeTable Identity))
lookupCompanyInUserTypeTable uTypeTbl companyId = do
  runSelectReturningOne $ select $ do
    uTypes <- all_ uTypeTbl
    guard_ $ _userType_userType uTypes ==. (val_ Admin)
    let CompanyInfoId mCid = _userType_companyID uTypes
        CompanyInfoId targetCid = companyId
    guard_ $ mCid ==. just_ (val_ targetCid)
    pure uTypes

getInviteLinkByCode
  :: Database Postgres db
  => PgTable Postgres db InviteLink
  -> T.Text
  -> Pg (Maybe (InviteLink Identity))
getInviteLinkByCode inviteTbl codeLink = do
  runSelectReturningOne $ select $ do
    filter_ (\link -> _inviteLink_code link ==. (val_ codeLink)) $ all_ inviteTbl

setLinkNumLeft
  :: PgTable Postgres db InviteLink
  -> T.Text
  -> Int64
  -> Pg ()
setLinkNumLeft inviteTbl codeLink numLeft = do
  runUpdate $ update
    inviteTbl
    (\inviteLink ->
       _inviteLink_numLeft inviteLink <-. (val_ $ Just numLeft)
    )
    (\inviteLink -> _inviteLink_code inviteLink ==. (val_ codeLink) &&. (isJust_ $ _inviteLink_numLeft inviteLink))

insertInviteLink
  :: PgTable Postgres db InviteLink
  -> PrimaryKey CompanyInfo Identity
  -> T.Text
  -> Maybe Int64
  -> Pg ()
insertInviteLink inviteTbl companyId code mNumLeft = runInsert $ insert inviteTbl $ insertExpressions
  [ InviteLink (val_ companyId) (val_ code) (val_ mNumLeft) ]
