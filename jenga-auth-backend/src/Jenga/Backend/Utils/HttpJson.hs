module Jenga.Backend.Utils.HttpJson where

-- import Backend.Utils.ErrorHandling
import Jenga.Backend.Utils.AuthHandlers
import Jenga.Backend.Utils.Snap
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.ErrorHandling
import Jenga.Backend.Utils.Email
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Common.Schema
import Jenga.Common.BeamExtras


import Rhyolite.Account
import Database.Beam.Postgres
import Database.Beam.Schema
import Web.ClientSession as CS
import Snap
import Snap.Extras (writeJSON)

import Control.Monad.Trans.Reader
import Control.Monad (void)
import Control.Exception.Lifted (catch, SomeException(..))
import Data.Pool
import Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS


-- | Rewrite with getRequestBodyJSON
withRequestBodyJSON
  :: forall cfg a m.
     ( Aeson.FromJSON a
     , MonadSnap m
     )
  => (a -> ReaderT cfg m ())
  -> ReaderT cfg m ()
withRequestBodyJSON withF = do
  raw <- getRequestBody
  case Aeson.eitherDecode raw :: Either String a of
    Left e -> liftSnap . writeJSON' $ (Left . BInvalidRequest $ T.pack e :: Either (BackendError ()) ())
    Right x -> withF x

withPublicJSONRequestResponse
  :: forall db be a b err cfg m n
  . ( Aeson.FromJSON a
    , Aeson.ToJSON b
    , Aeson.ToJSON err
    , Show err
    , SpecificError (BackendError err)
    , MonadSnap m
    , Database Postgres db
    , HasConfig cfg AdminEmail
    , HasConfig cfg CS.Key
    , HasConfig cfg AuthCookieName
    , HasConfig cfg (Pool Connection)
    , HasJengaTable Postgres db UserTypeTable
    , HasJengaTable Postgres db LogItemRow
    , HasJengaTable Postgres db SendEmailTask
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => (a -> ReaderT cfg m (Either (BackendError err) b))
  -> ReaderT cfg m ()
withPublicJSONRequestResponse = void . withJSONRequestResponse @db
    -- \case
    -- Right _ -> pure ()
    -- Left err -> emailGalenWithEnv . LT.fromStrict $ err
  -- x :: Either ApiError b <- withJSONRequestResponse (withF )

  -- when (isLeft x) $  do
  --   emailGalenWithEnv $ LT.fromStrict . T.pack $ show x
  --   -- |^emailGalen emailConfig (LT.fromStrict . T.pack $ show x) -- REIMPLEMENT
  --   pure ()

withConstrainedPrivateJSONRequestResponse
  :: forall db be a b m e cfg n
  . ( FromJSON a
    , ToJSON b
    , ToJSON e
    , Show b
    , Show e
    , SpecificError (BackendError e)
    , MonadSnap m
    , Database Postgres db
    , HasConfig cfg CS.Key
    , HasConfig cfg AuthCookieName
    , HasConfig cfg AdminEmail
    , HasConfig cfg (Pool Connection)
    , HasJengaTable Postgres db UserTypeTable
    , HasJengaTable Postgres db LogItemRow
    , HasJengaTable Postgres db SendEmailTask
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => NE.NonEmpty UserType
  -> (Id Account -> a -> ReaderT cfg m (Either (BackendError e) b))
  -> ReaderT cfg m ()
withConstrainedPrivateJSONRequestResponse allowedUserTypes withF = do
  csk <- asksM
  dbConn <- asksM -- Cfg _dbPool
  constrainedPrivateRoute @db dbConn csk allowedUserTypes $ \acctId -> do
    withJSONRequestResponse @db (withF acctId)
    -- when (isLeft x) $ liftIO $ do
    --   -- |^ FIX emailGalen emailConfig (LT.fromStrict . T.pack $ show acctId <> show x)
    --   print (LT.fromStrict . T.pack $ show acctId <> show x)

withDependentPrivateJSONRequestResponse
  :: forall db be a b m e cfg n
  . ( FromJSON a
    , ToJSON b
    , ToJSON e
    , Show b
    , Show e
    , SpecificError (BackendError e)
    , MonadSnap m
    , Database Postgres db
    , HasConfig cfg CS.Key
    , HasConfig cfg AuthCookieName
    , HasConfig cfg (Pool Connection)
    , HasConfig cfg AdminEmail
    , HasJengaTable Postgres db UserTypeTable
    , HasJengaTable Postgres db LogItemRow
    , HasJengaTable Postgres db SendEmailTask
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => (UserType -> Id Account -> a -> ReaderT cfg m (Either (BackendError e) b))
  -> ReaderT cfg m ()
withDependentPrivateJSONRequestResponse withF = do
  csk <- asksM -- Cfg _clientSessionKey
  dbConn <- asksM -- Cfg _dbPool
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  authCookieName <- asksM
  dependentPrivateRoute uTypeTbl authCookieName dbConn csk $ \userType acctId -> do
    withJSONRequestResponse @db (withF userType acctId)

withAnyAuthStateJSONRequestResponse
  :: forall db be a b err cfg m n
  . ( FromJSON a
    , ToJSON b
    , ToJSON err
    , Show err
    , SpecificError (BackendError err)
    , MonadSnap m
    , Database Postgres db
    , HasConfig cfg AdminEmail
    , HasConfig cfg CS.Key
    , HasConfig cfg AuthCookieName
    , HasConfig cfg (Pool Connection)
    , HasJengaTable Postgres db UserTypeTable
    , HasJengaTable Postgres db LogItemRow
    , HasJengaTable Postgres db SendEmailTask
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => (Maybe (Id Account) -> a -> ReaderT cfg m (Either (BackendError err) b))
  -> ReaderT cfg m ()
withAnyAuthStateJSONRequestResponse withF = do
  eAcct <- getAccountIdFromCookies
    :: ReaderT cfg m (Either (BackendError ()) AccountId)
  let mAcct = either (const Nothing) Just eAcct
  void $ withJSONRequestResponse @db (withF mAcct)

-- | This function is meant to be completely independent of Authentication, so that it can be
-- | wrapped by different auth schemes
withJSONRequestResponse
  :: forall db be a b m err cfg n
  . ( FromJSON a
    , ToJSON b
    , ToJSON err
    , MonadSnap m
    , Show err
    , SpecificError (BackendError err)
    , HasConfig cfg (Pool Connection)
    , HasConfig cfg AdminEmail
    , HasJengaTable Postgres db LogItemRow
    , HasJengaTable Postgres db SendEmailTask
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => (a -> ReaderT cfg m (Either (BackendError err) b))
  -> ReaderT cfg m (Either (BackendError err) b)
withJSONRequestResponse withF = do
  raw <- getRequestBody
  x :: Either (BackendError e) b <- case Aeson.eitherDecode raw :: Either String a of
    Left e -> pure $ Left . BInvalidRequest $ T.pack e
    Right good -> catch (withF good) (\(SomeException e) -> pure $ Left . BException $ T.pack . show $ e)
  writeJSON x
  reportWhenError @db x
  --pure x

withPrivateRequestResponse
  :: forall db cfg be a e m n.
     ( Show a
     , Show e
     , SpecificError (BackendError e)
     , MonadSnap m
     , HasConfig cfg AdminEmail
     , HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     , HasJengaTable Postgres db LogItemRow
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => Id Account
  -> (Id Account -> LBS.ByteString -> ReaderT cfg m (Either (BackendError e) a))
  -> ReaderT cfg m (Either (BackendError e) a)
withPrivateRequestResponse id_ withF = withPrivateErrorHandling @db id_ withF

withPrivateErrorHandling
  :: forall db cfg be a e m n.
     ( Show a
     , Show e
     , SpecificError (BackendError e)
     , MonadSnap m
     , HasConfig cfg AdminEmail
     , HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     , HasJengaTable Postgres db LogItemRow
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => Id Account
  -> (Id Account -> LBS.ByteString -> ReaderT cfg m (Either (BackendError e) a))
  -> ReaderT cfg m (Either (BackendError e) a)
withPrivateErrorHandling acctID withF = do
  raw <- getRequestBody
  x <- catch (withF acctID raw) (\(SomeException e) -> pure $ Left . BException $ T.pack . show $ e)
  reportWhenError @db x
  -- when (isLeft x) $ liftIO $ do
  --   -- |^ FIX emailGalen emailConfig (LT.fromStrict . T.pack $ show x)
  --   print (LT.fromStrict . T.pack $ show x)
  --   logFor acctID (LT.fromStrict . T.pack $ show x)
  -- pure x

-- | NOTE: not yet in use, may not be ever needed but do keep as generic interface
withPublicRequestResponse
  :: forall db cfg be n e m a.
     ( Show a
     , Show e
     , SpecificError (BackendError e)
     , MonadSnap m
     , HasConfig cfg AdminEmail
     , HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     , HasJengaTable Postgres db LogItemRow
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => (LBS.ByteString -> ReaderT cfg m (Either (BackendError e) a))
  -> ReaderT cfg m (Either (BackendError e) a)
withPublicRequestResponse withF = withPublicErrorHandling @db withF
-- | NOTE: not yet in use, may not be ever needed but do keep as generic interface
withPublicErrorHandling
  :: forall db cfg be a e m n.
     ( Show a
     , Show e
     , SpecificError (BackendError e)
     , MonadSnap m
     , HasConfig cfg AdminEmail
     , HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     , HasJengaTable Postgres db LogItemRow
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => (LBS.ByteString -> ReaderT cfg m (Either (BackendError e) a))
  -> ReaderT cfg m (Either (BackendError e) a)
withPublicErrorHandling withF = do
  raw <- getRequestBody
  x <- catch (withF raw) (\(SomeException e) -> pure $ Left . BException $ T.pack . show $ e)
  reportWhenError @db x
  -- when (isLeft x) $ liftIO $ do
  --   -- |^ FIX emailGalen emailConfig (LT.fromStrict . T.pack $ show x)
  --   logNoAuth (LT.fromStrict . T.pack $ show x)
  -- pure x

-- TODO(Galen): use requestBodyJSON
theoreticalJSONMonad
  :: forall a b m cfg.
     (FromJSON a, ToJSON b, MonadSnap m)
  => (a -> ReaderT cfg m (Either ApiError b))
  -> ReaderT cfg m (Either ApiError b)
theoreticalJSONMonad withF = do
  raw <- getRequestBody
  x :: Either ApiError b <- case Aeson.eitherDecode raw :: Either String a of
    Left e -> pure $ Left $ T.pack e
    Right good -> catch (withF good) (\(SomeException e) -> pure $ Left $ T.pack . show $ e)
  writeJSON x
  pure x


-- <<<<<<< HEAD
-- =======
--   mCookie <- getCookie $ T.encodeUtf8 authCookieName
--   case mCookie of
--     Nothing -> liftSnap $ writeJSON' $ Left "No cookie found"
--     Just cookie -> do
--       let
--         removeQuotes = T.init . (T.drop 1) -- TODO: why does this happen ?
--         signed :: Signed (PrimaryKey Rhyolite.Account.Account Identity)
--         signed = Signed . removeQuotes . T.decodeUtf8 . (fromRight undefined) . B64.decode . cookieValue $ cookie
--       case readSignedWithKey key signed of
--         Nothing -> liftSnap $ writeJSON'' $ Left "couldnt read key"
--         Just (AccountId (SqlSerial int64)) -> do
--           _ <- fma $ AcctID $ fromIntegral int64
--           pure ()

-- -- | Is copy of privateRoute with allowed users
-- dependentPrivateRoute
--   :: forall m a. (MonadSnap m)
--   => ConnectionPool
--   -> CSK.Key
--   -> (UserType -> AcctID -> m a)
--   -> m ()
-- dependentPrivateRoute dbConn key fma = do
--   mCookieAID <- getCookie $ T.encodeUtf8 authCookieName
--   case mCookieAID of
--     Nothing -> liftSnap $ writeJSON' $ (Left "No cookie found" :: Either T.Text ())
--     Just cookieAID -> do
--       let
--         removeQuotes = T.init . (T.drop 1) -- TODO: why does this happen ?
--         signed :: Signed (PrimaryKey Account Identity)
--         signed = Signed . removeQuotes . T.decodeUtf8 . (fromRight undefined) . B64.decode . cookieValue $ cookieAID
--       case readSignedWithKey key signed of
--         Nothing -> liftSnap $ writeJSON' $ (Left "couldnt read key" :: Either T.Text ())
--         -- We still have this UserType on the frontend so that
--         -- we can run conditional logic on what to show the user
--         -- but for safety we ask the type ourselves as the user could just enter in a fake one
--         Just (AccountId (SqlSerial int64)) -> do
--           userType' <- runSerializable dbConn $ getUserType (AcctID $ fromIntegral int64)
--           case userType' of
--             Nothing -> liftSnap $ writeJSON' $ (Left "No user type found" :: Either T.Text ())
--             Just userType -> do
--               _ <- fma userType $ AcctID $ fromIntegral int64
--               pure ()

-- -- NOTE: could easily generalize this if we ever have other signed data in headers?
-- getAccountIdFromCookies :: MonadSnap m => ReaderT cfg m (Either T.Text AccountId)
-- getAccountIdFromCookies = do
--   cskey <- asksCfg _clientSessionKey
--   mCookie <- getCookie $ T.encodeUtf8 authCookieName
--   case mCookie of
--     Nothing -> pure $ Left "No Cookie Header found"
--     Just cookieSignedId -> do
--       let
--         removeQuotes = T.init . (T.drop 1) -- TODO: why does this happen ?
--         eithDecodedB64 = B64.decode . cookieValue $ cookieSignedId
--       case eithDecodedB64 of
--         Left decodeErr -> pure $ Left $ "Base64 Decode Error: " <> T.pack decodeErr
--         Right decodedB64 -> do
--           let
--             -- for type annotation
--             signed :: Signed (PrimaryKey Account Identity)
--             signed = Signed . removeQuotes . T.decodeUtf8 $ decodedB64
--           case readSignedWithKey cskey signed of
--             Nothing -> pure $ Left "Couldn't read key"
--             Just acctId -> pure $ Right acctId


-- getRequestBodyJSON :: forall a m. (FromJSON a, MonadSnap m) => m (Either String a)
-- getRequestBodyJSON = Aeson.eitherDecode <$> getRequestBody


-- getRequestBody :: MonadSnap m => m LBS.ByteString
-- getRequestBody = LBS.fromChunks <$> runRequestBody Streams.toList

-- writeJSON' :: MonadSnap m => Either T.Text () -> m ()
-- writeJSON' = writeJSON

-- -- | is copy of privateRoute with allowed users
-- constrainedPrivateRoute
--   :: forall m a. (MonadSnap m)
--   => ConnectionPool
--   -> CSK.Key
--   -> NE.NonEmpty UserType
--   -> (AcctID -> m a)
--   -> m ()
-- constrainedPrivateRoute dbConn key userTypesAllowed fma = do
--   mCookieAID <- getCookie $ T.encodeUtf8 authCookieName
--   case mCookieAID of
--     Nothing -> liftSnap $ writeJSON' $ (Left "No cookie found" :: Either T.Text ())
--     Just cookieAID -> do
--       let
--         removeQuotes = T.init . (T.drop 1) -- TODO: why does this happen / why do we need this drop?
--         signed :: Signed (PrimaryKey Account Identity)
--         signed = Signed . removeQuotes . T.decodeUtf8 . (fromRight undefined) . B64.decode . cookieValue $ cookieAID
--       case readSignedWithKey key signed of
--         Nothing -> liftSnap $ writeJSON' $ (Left "couldnt read key" :: Either T.Text ())
--         -- We still have this UserType on the frontend so that
--         -- we can run conditional logic on what to show the user
--         -- but for safety we ask the type ourselves as the user could just enter in a fake one
--         Just (AccountId (SqlSerial int64)) -> do
--           userType' <- runSerializable dbConn $ getUserType (AcctID $ fromIntegral int64)
--           case userType' of
--             Nothing -> liftSnap $ writeJSON' $ (Left "No user type found" :: Either T.Text ())
--             Just userType -> do
--               case elem userType userTypesAllowed of
--                 False -> pure ()
--                 True -> do
--                   _ <- fma $ AcctID $ fromIntegral int64
--                   pure ()



-- privateRouteJSONOut
--   :: forall m a. (ToJSON a, MonadSnap m)
--   => CSK.Key
--   -> (AcctID -> m (Either ApiError a))
--   -> m ()
-- privateRouteJSONOut key fma = do
--   let writeJSON'' :: forall x. (ToJSON x) => Either ApiError x -> Snap ()
--       writeJSON'' = writeJSON
--   mCookie <- getCookie $ T.encodeUtf8 authCookieName
--   case mCookie of
--     Nothing -> liftSnap $ writeJSON'' $ ( Left "No cookie found"  :: Either T.Text a)
--     Just cookieAID -> do
--       let
--         removeQuotes = T.init . (T.drop 1) -- TODO: why does this happen ?
--         signed :: Signed (PrimaryKey Account Identity)
--         signed = Signed . removeQuotes . T.decodeUtf8 . (fromRight undefined) . B64.decode . cookieValue $ cookieAID
--       case readSignedWithKey key signed of
--         Nothing -> liftSnap $ writeJSON'' $ (Left "couldnt read key" :: Either T.Text a)
--         Just (AccountId (SqlSerial int64)) -> do
--           a <- fma $ AcctID $ fromIntegral int64
--           liftSnap $ writeJSON'' a

-- >>>>>>> origin/master
