module Jenga.Backend.Handlers.Auth.OrgBased.AddUsers where

-- import Backend.DB.OrgBased
-- import Backend.Config
-- import Common.Types
-- import Backend.Utils.Account
import Jenga.Backend.Utils.Account
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.Email
import Jenga.Backend.DB.OrgBased
import Jenga.Common.Errors
import Jenga.Common.BeamExtras
import Jenga.Common.Schema
import Jenga.Common.Auth

import Rhyolite.Account
import Database.Beam.Schema
import Database.Beam.Postgres
import Snap

import Data.Pool
import Web.ClientSession as CS
import Data.Signed
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Applicative (some)
import Text.Parsec
import Text.Email.Validate
import qualified Data.Text.Encoding as T
import qualified Data.Text as T


type AddUsersConstraint db beR cfg be m n frontendRoute =
  --forall db beR cfg be m n frontendRoute x.
     ( MonadIO m
     , MonadSnap m
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


addUsersHandler
  :: forall db beR cfg be m n frontendRoute x.
     ( MonadIO m
     , MonadSnap m
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
  => Id Account
  -> T.Text
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -- ^ Email to send user
  -> ReaderT cfg m (Either (BackendError AddUsersError) AddUsersResult)
addUsersHandler acctID emails resetRoute mkEmail = do
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  case sepByCommas emails of
    Left _ -> pure $ Left . BUserError $ NoCommas -- "Error reading list, please ensure all emails are separated by commas"
    Right rawEmails -> do
      setTimeout $ length rawEmails + 10
      liftIO $ print rawEmails
      let trimmedEmails = filter (not . null) $ fmap (T.unpack . T.strip . T.pack) rawEmails
      case trimmedEmails of
        [] -> pure $ Left . BUserError $ NoUsersGiven
        _ -> do
          let validated = fmap (\e -> (T.pack e, validate . T.encodeUtf8 . T.pack $ e)) trimmedEmails
              badEmails = [raw | (raw, Left _) <- validated]
              goodEmails = [addr | (_, Right addr) <- validated]
          case badEmails of
            (_:_) -> pure $ Left . BUserError $ InvalidEmail_AddUser badEmails
            [] -> do
              mCompanyId <- withDbEnv $ getCompanyId uTypeTbl acctID
              case mCompanyId of
                Nothing -> pure $ Left . BUserError $ NoOrgCode $ T.pack . show $ acctID
                Just companyId -> do
                  results <- forM goodEmails $ \email -> do
                    let emailText = T.decodeUtf8 . toByteString $ email
                    createNewAccountWithSetupEmail @db @beR email (IsGroupUser email companyId) resetRoute mkEmail >>= \case
                      Right _ -> pure (AddUser_Added emailText)
                      Left bErr -> case bErr of
                        BUserError AccountExists -> pure (AddUser_Skipped emailText)
                        _ -> pure (AddUser_Failed emailText)
                  let added = [e | AddUser_Added e <- results]
                      skipped = [e | AddUser_Skipped e <- results]
                      failed = [e | AddUser_Failed e <- results]
                  pure $ Right $ AddUsersResult (length added) skipped failed

sepByCommas :: T.Text -> Either ParseError [String]
sepByCommas = parse p ""
  where
    p = sepBy (some $ noneOf [',', '\n', '\r', ' ']) (skipMany1 $ oneOf [',', '\n', '\r', ' '])
