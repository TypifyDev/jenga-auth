module Jenga.Backend.Handlers.Auth.ResetPassword where

import Jenga.Backend.DB.Auth

import Jenga.Backend.Utils.Query
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Email
import Jenga.Backend.Utils.Cookies (addAuthCookieHeader)
import Jenga.Common.Schema
import Jenga.Common.BeamExtras
import Jenga.Common.Errors
import Jenga.Common.Auth

import Rhyolite.Account
import Database.Beam.Schema
import Database.Beam.Postgres

import qualified Snap
import Network.Mail.Mime
import Data.Pool
import Web.ClientSession as CS
import Data.Signed.ClientSession
import Data.Signed
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import qualified Data.Text as T


resetPasswordHandler
  :: forall db cfg x m n.
     ( MonadIO m
     , Snap.MonadSnap m
     , Database Postgres db
     , HasConfig cfg AdminEmail
     , HasConfig cfg CS.Key
     , HasConfig cfg DomainOption
     , HasConfig cfg AuthCookieName
     , HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db Account
     , HasJsonNotifyTbl Postgres SendEmailTask n
     )
  => (Signed PasswordResetToken, T.Text)
  -> (UserType -> PasswordState -> MkEmail x)
  -> ReaderT cfg m (Either (BackendError ResetPasswordError) (Signed (Id Account), UserType))
resetPasswordHandler (signedToken, newPass) chooseWelcomeLetter = do
  (acctsTbl :: PgTable Postgres db Account) <- asksTableM
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM

  dbConnection <- asksM -- Cfg _dbPool
  csk <- asksM -- Cfg _clientSessionKey
  case readSignedWithKey csk signedToken of
    Nothing -> pure . Left . BUserError $ InvalidToken -- "Invalid token"
    Just (PasswordResetToken (accountID, nonce)) -> do
      (rsetPass, acct, userType) <- runSerializable dbConnection $ do
        accountOld <- selectAccountData acctsTbl accountID
        resetPass <- resetPassword' acctsTbl accountID nonce (newPass)
        userType <- getUserType uTypeTbl accountID
        pure (resetPass, accountOld, userType)
      case rsetPass of
        Left e -> pure $ Left e
        Right aid -> case acct of
          Nothing -> pure . Left . BCritical $ CouldntRetrieveAccount
          Just acct' -> do
            let toPasswordState = \case
                  False -> NoPassword
                  True -> HasPassword
            case userType of
              Nothing -> pure . Left . BCritical $ Reset_NoUserTypeFound
              Just uType -> do
                let
                  to = Address
                    { addressName = Nothing
                    , addressEmail = _account_email acct'
                    }
                  passwordState = toPasswordState . isJust . _account_password $ acct'
                  --(Subject subject, email) = chooseWelcomeLetter uType . toPasswordState . isJust . _account_password $ acct'
                newMkEmailHtml @db [to] (chooseWelcomeLetter uType passwordState) >>= \case
                  Left _ -> pure . Left . BCritical $ Reset_NoEmailSent
                  Right () -> do
                    authCookieName <- getAuthCookieName <$> asksM
                    addAuthCookieHeader authCookieName aid
                    signed <- liftIO $ signWithKey csk ( aid )
                    pure $ Right (signed, uType)

newtype Subject = Subject { getSubject :: T.Text }
data PasswordState = NoPassword | HasPassword deriving Eq
