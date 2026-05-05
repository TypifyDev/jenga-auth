module Jenga.Backend.Handlers.Auth.UserSignup where

import Jenga.Backend.Utils.Account
import Jenga.Backend.Utils.Email
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Common.Schema

import Rhyolite.Account
import Network.Mail.Mime
import Database.Beam
import Database.Beam.Postgres

import Data.Pool
import Web.ClientSession as CS
import Data.Signed
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Text.Email.Validate as EmailValidate
import qualified Data.Text.Encoding as T

-- userSignup
--   :: MonadIO m
--   => T.Text -- ^ email subject
--   -> T.Text -- ^ email address
--   -> frontendRoute (Signed PasswordResetToken)
--   -> (T.Text -> StaticWidget x ())
--   -> ReaderT cfg m (Either (BackendError UserSignupError) ())
-- userSignup subject address resetR mkEmail = userSignup'
--   subject
--   address
--   resetR
--   mkEmail


--let mkBody = body <> "\n" <> link -- plaintext emali
userSignupHandler
  :: forall db beR be frontendRoute m cfg x n.
     ( MonadIO m
     , Database Postgres db
     , HasConfig cfg CS.Key
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasConfig cfg BaseURL
     , HasConfig cfg AdminEmail
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => EmailValidate.EmailAddress -- ^ email address
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)  -- ^ email body
  -> ReaderT cfg m (Either (BackendError UserSignupError) ())
userSignupHandler email resetRoute mkBody = do
  case EmailValidate.emailAddress (EmailValidate.toByteString email) of
    Nothing -> pure . Left . BUserError $ BadSignupEmail
    Just emailParsed -> do
      createNewAccount @db @beR (emailParsed) IsSelf resetRoute >>= \case
        Left e -> pure $ Left e
        Right link -> do
          let
            to = Address
              { addressName = Nothing
              , addressEmail = T.decodeUtf8 . toByteString $ email
              }
          x <- newMkEmailHtml @db [to] $ mkBody link
          pure $ first (\_ -> BCritical NoEmailSent) x
