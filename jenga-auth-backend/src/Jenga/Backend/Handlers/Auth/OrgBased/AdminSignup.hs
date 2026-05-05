module Jenga.Backend.Handlers.Auth.OrgBased.AdminSignup where

import Jenga.Backend.Utils.Account
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Email
import Jenga.Common.Errors
import Jenga.Common.Company
import Jenga.Common.Schema
import Jenga.Common.Auth

import Network.Mail.Mime
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Rhyolite.Account

import Web.ClientSession as CS
import Data.Pool
import Data.Signed
import Control.Monad.Trans.Reader
import Text.Email.Validate
import Data.Maybe (listToMaybe)
import qualified Data.Text.Encoding as T

adminSignupHandler
  :: forall db beR frontendRoute cfg m x n.
     ( MonadIO m
     , Database Postgres db
     , HasConfig cfg CS.Key
     , HasConfig cfg CompanySignupCode
     , HasConfig cfg BaseURL
     , HasConfig cfg AdminEmail
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     , HasJengaTable Postgres db CompanyInfo
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl Postgres SendEmailTask n
     )
  => NewCompanyEmail
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -> ReaderT cfg m (Either (BackendError AdminSignupError) ())
adminSignupHandler (NewCompanyEmail email orgName code) resetRoute mkEmail = do
  matchesCompanyCodeEnv code >>= \case
    False -> pure $ Left . BUserError $ InvalidAdminCode
    True -> do
      (companyTbl :: PgTable Postgres db CompanyInfo) <- asksTableM
      mCompanyId <- withDbEnv $ do
        results <- runInsertReturningList $ insert companyTbl $ insertExpressions
          [ CompanyInfo default_ (val_ orgName) (val_ Nothing) (val_ Nothing) (val_ Nothing) (val_ Nothing) ]
        pure $ primaryKey <$> listToMaybe results
      case mCompanyId of
        Nothing -> pure $ Left . BCritical $ CompanyCreationFailed
        Just companyId ->
          createNewAccount @db @beR email (IsCompany companyId) resetRoute >>= \case
            Left beErr -> pure $ Left $ fmap AdminSignupError $ beErr
            Right link -> do
              let
                to = Address
                     { addressName = Nothing
                     , addressEmail = T.decodeUtf8 . toByteString $ email
                     }
              eRes <- newMkEmailHtml @db [to] $ mkEmail link
              case eRes of
                Left _ -> pure $ Left . BCritical . AdminSignupError $ NoEmailSent
                Right () -> pure $ Right ()

--  do
--                 Rfx.el "div" $ do
--                   Rfx.el "div" $ Rfx.text "New account creation requested"
--                   Rfx.el "div" $ Rfx.text "go the following address to set your password:"
--                   Rfx.el "div" $ Rfx.text link
--                   Rfx.el "div" $ Rfx.text ""
--                   Rfx.el "div" $ Rfx.text "if you didn't request this action, please ignore this email."

      -- (isNew, aid) <- withDbEnv $ do
      --   ensureAccountExists' acctTbl $ T.decodeUtf8 . toByteString $ email
      -- case isNew of
      --   False -> pure $ Left . BUserError $ AlreadySignedUp
      --   True -> do
      --     mNonce <- withDbEnv $ do
      --       putAccountRelations uTypeTbl orgTbl aid (IsCompany orgName)
      --       newNonce acctTbl aid
      --     case mNonce of
      --       Nothing -> pure $ Left . BCritical $ FailedMkNonce_AdminSignup
      --       Just noncense -> do
      --         csk <- asksM
      --         resetToken <- withDbEnv $ passwordResetToken csk aid noncense
      --         link <- renderFullRouteFE @beR $ resetRoute :/ resetToken
      --         let
      --           to = Address
      --                { addressName = Nothing
      --                , addressEmail = T.decodeUtf8 . toByteString $ email
      --                } -- recipients Address
      --         eRes <- newEmailHtml @db [to] "Admin Email Confirmation" $ mkEmail link
      --         case eRes of
      --           Left _ -> pure $ Left . BCritical $ FailedMkEmail_AdminSignup
      --           Right () -> do
      --             pure $ Right ()
