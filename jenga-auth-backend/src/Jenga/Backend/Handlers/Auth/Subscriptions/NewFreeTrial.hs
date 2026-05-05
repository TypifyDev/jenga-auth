module Jenga.Backend.Handlers.Auth.Subscriptions.NewFreeTrial where

import Jenga.Backend.DB.Auth
import Jenga.Backend.DB.Subscriptions
import Jenga.Backend.Utils.Account
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Email
import Jenga.Common.Errors
import Jenga.Common.Schema
import Jenga.Common.Auth

import Database.Beam.Postgres
import Database.Beam.Schema
import Rhyolite.Account

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Signed
import Data.Pool
import Web.ClientSession as CS
import Text.Email.Validate
import qualified Data.Text as T
import qualified Data.Text.Encoding as T



newFreeTrialHandler
  :: forall db beR cfg be m n frontendRoute x.
     ( MonadIO m
     --, EmailM cfg db m n be
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
     , HasJengaTable Postgres db FreeTrial
     )
  => (Maybe T.Text, Email)
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -> ReaderT cfg m (Either (BackendError FreeTrialError) ())
newFreeTrialHandler (mCode,email) resetRoute mkEmail = do
  (freeTrialTbl :: PgTable Postgres db FreeTrial) <- asksTableM
  (acctsTbl :: PgTable Postgres db Account) <- asksTableM
  case validate . T.encodeUtf8 . unEmail $ email of
    Left _ -> pure $ Left . BUserError $ InvalidEmail_FreeTrial
    Right validatedEmail -> do
      createNewAccountWithSetupEmail @db @beR validatedEmail IsSelf resetRoute mkEmail >>= \case
        Left e -> pure $ Left $ FreeTrial_Signup <$> e
        Right () -> do
          (withDbEnv $ getUserByEmail acctsTbl $ unEmail email) >>= \case
            Nothing -> pure $ Left . BCritical $ NoUserForFreeTrial-- "Error creating user: on get user: not found"
            Just user -> do
              withDbEnv $ putNewFreeTrial freeTrialTbl (pk user) mCode
              pure $ Right ()
