module Jenga.Backend.Utils.Account (createNewAccount, createNewAccountWithSetupEmail) where

import Jenga.Backend.DB.Auth
import Jenga.Backend.DB.OrgBased
import Jenga.Backend.Utils.HasTable (PgTable, HasJengaTable, asksTableM, withDbEnv)
import Jenga.Common.Schema
import Control.Monad.Trans.Reader

import Web.ClientSession as CS

import Jenga.Backend.Utils.Email
import Jenga.Backend.Utils.HasConfig
import Jenga.Common.Errors
import Jenga.Common.Auth


import Rhyolite.Account
import Rhyolite.Backend.Account
import Obelisk.Route
import Database.Beam.Postgres
import Database.Beam
import Network.Mail.Mime
import Data.Signed

import Data.Pool
import Data.Bifunctor
import Text.Email.Validate
import qualified Data.Text.Encoding as T



createNewAccount
  :: forall db beR frontendRoute m cfg .
     ( MonadIO m
     , Database Postgres db
     , HasConfig cfg CS.Key
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasConfig cfg BaseURL
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     )
  => EmailAddress
  -> IsUserType
  -> frontendRoute (Signed PasswordResetToken)
  -> ReaderT cfg m (Either (BackendError UserSignupError) Link)
createNewAccount email isUserType resetRoute = do
  csk <- asksM
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  (orgTbl :: PgTable Postgres db OrgOwnedUsers) <- asksTableM
  (acctsTbl :: PgTable Postgres db Account) <- asksTableM
  (accountsTable :: PgTable Postgres db Account) <- asksTableM
  (withDbEnv $ ensureAccountExists' acctsTbl $ T.decodeUtf8 . toByteString $ email) >>= \case
    Left (EnsureAccount_InsertReturnedUnexpectedRows n) -> pure $ Left . BCritical $ AccountInsertFailed n
    Right (False, _) -> pure $ Left . BUserError $ AccountExists
    Right (True, aid) -> do
      (withDbEnv $ putAccountRelations uTypeTbl orgTbl aid isUserType) >>= \case
        Left noOrgErr -> pure . Left . BCritical $ noOrgErr
        Right () -> do
          (withDbEnv $ newNonce accountsTable aid) >>= \case
            Nothing -> pure $ Left . BUserError $ FailedMakeResetToken
            Just nonce -> do
              token <- withDbEnv $ passwordResetToken csk aid nonce
              resetLink <- renderFullRouteFE @beR $ ((resetRoute :/ token) :: R frontendRoute)
              pure $ Right resetLink

createNewAccountWithSetupEmail
  :: forall db beR cfg be m n frontendRoute x.
     ( MonadIO m
     , HasJsonNotifyTbl be SendEmailTask n
     , Database Postgres db
     , HasConfig cfg AdminEmail
     , HasConfig cfg CS.Key
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasConfig cfg BaseURL
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     , HasJengaTable Postgres db SendEmailTask
     )
  => EmailAddress
  -> IsUserType
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -> ReaderT cfg m (Either (BackendError UserSignupError)  ())
createNewAccountWithSetupEmail email isUserType resetRoute mkEmail = do
  (createNewAccount @db @beR email isUserType resetRoute) >>= (\case
    Left e -> pure $ Left e
    Right link_ -> do
      x <- newMkEmailHtml @db [to] $ mkEmail link_
      pure $ first (\_ -> BCritical NoEmailSent) x)
  where
    to = Address
      { addressName = Nothing
      , addressEmail = T.decodeUtf8 . toByteString $ email
      }
