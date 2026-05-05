module Jenga.Backend.Handlers.Auth.Invite where

import Jenga.Backend.Handlers.Auth.UserSignup (userSignupHandler)
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Email
import Jenga.Backend.DB.Instances ()
import Jenga.Common.BeamExtras
import Jenga.Common.Errors
import Jenga.Common.Schema
import Jenga.Common.Auth

import Rhyolite.Account
import Database.PostgreSQL.Simple
import Database.Beam.Postgres
import Database.Beam.Schema
import Database.Beam.Query

import Web.ClientSession as CS
import Data.Pool
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Signed
import Data.Bifunctor
import Data.Maybe
import Text.Email.Validate as EmailValidate
import qualified Data.Text as T

inviteHandler
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
  => EmailValidate.EmailAddress
  -> Id Account
  -> frontendRoute (Signed PasswordResetToken)
  -> (T.Text -> Link -> MkEmail x)  -- ^ email body
  -> ReaderT cfg m (Either (BackendError InviteError) ())
inviteHandler email inviter resetRoute mkBody = do
  (acctTbl :: PgTable Postgres db Account) <- asksTableM
  user <- (fmap.fmap) _account_email $ withDbEnv $ runSelectReturningOne $ lookup_ acctTbl inviter
  signupRes <- userSignupHandler @db @beR
    email
    resetRoute
    (\link_ -> mkBody (fromMaybe "another user" user) link_)
  pure $ first (fmap InviteSignupFailed) signupRes
