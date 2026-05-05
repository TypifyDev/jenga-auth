module Jenga.Backend.Handlers.Auth.OrgBased.RedeemLink where

import Jenga.Backend.DB.OrgBased
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Account
import Jenga.Backend.Utils.Email
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Common.Schema

import Rhyolite.Account
import Database.Beam.Postgres
import Database.Beam.Schema

import Web.ClientSession as CS
import Data.Signed
import Data.Pool
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Text.Email.Validate
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

redeemLinkHandler
  :: forall db beR cfg frontendRoute be m x n.
     ( MonadIO m
     , Database Postgres db
     , HasConfig cfg AdminEmail
     , HasConfig cfg BaseURL
     , HasConfig cfg CS.Key
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasJengaTable Postgres db InviteLink
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => (T.Text, Email)
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -> ReaderT cfg m (Either (BackendError RedeemLinkError) ())
redeemLinkHandler (codeLink, Email email) resetRoute mkEmail = do
  (inviteTbl :: PgTable Postgres db InviteLink) <- asksTableM

  withDbEnv (getInviteLinkByCode inviteTbl codeLink) >>= \case
    Nothing -> pure $ Left . BUserError $ NoOrgCode_RedeemLink
    Just (InviteLink companyId code' mNLeft)
      | mNLeft == (Just 0) -> pure $ Left . BUserError $ CreditsDepleted (T.pack $ show companyId)
      | otherwise -> do
          case validate . T.encodeUtf8 $ email of
            Left _ -> pure $ Left . BUserError $ InvalidEmail_RedeemLink
            Right email' -> do
              createNewAccountWithSetupEmail @db @beR email' (IsGroupUser email' companyId) resetRoute mkEmail >>= \case
                Left e -> pure $ Left $ RedeemLink_Signup <$> e
                Right () -> do
                  case mNLeft of
                    Nothing -> pure ()
                    Just nLeft -> withDbEnv $ setLinkNumLeft inviteTbl code' (nLeft - 1)
                  pure $ Right ()
