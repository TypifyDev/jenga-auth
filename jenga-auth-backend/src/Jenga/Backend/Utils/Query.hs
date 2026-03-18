{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Jenga.Backend.Utils.Query where
-- we need the -fno-warn-orphans so that we can keep certain types in Common but use them in restricted database modules


-- import Snap (Snap, MonadSnap, liftSnap, getCookie, cookieValue)
import Database.PostgreSQL.Simple.Transaction (withTransactionSerializable)
import Database.PostgreSQL.Simple.Beam ()
import Data.Pool (Pool, withResource)
import qualified Data.Text.Encoding as T
import Database.Beam

import Control.Monad
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.Class
import qualified Data.Text as T

-- | Untyped queries may be necessary for migrating from Table version to another
-- | so this function exists mainly for the `preHook` function used for runAceMigration
exec_ :: String -> Pg ()
exec_ = void . execute_ . Query . T.encodeUtf8 . T.pack

-- | Check if the database has no public tables (brand new / fresh)
isFreshDb :: Pg Bool
isFreshDb = do
  [Only tableCount] <- query_ "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = 'public'"
  pure (tableCount == (0 :: Int))

-- | Run a Pg action only if a specific table exists in the public schema
whenTableExists :: String -> Pg () -> Pg ()
whenTableExists tbl action = do
  [Only tableExists] <- query_ $ Query $ T.encodeUtf8 $ T.pack $
    "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = 'public' AND table_name = '" <> tbl <> "')"
  when tableExists action

runDb :: MonadIO m => Pool Connection -> Pg a -> m a
runDb pool a = liftIO $ withResource pool $ \conn ->
  withTransactionSerializable conn $ runBeamPostgres conn a

runDbTrace :: MonadIO m => Pool Connection -> Pg a -> m a
runDbTrace pool a = liftIO $ withResource pool $ \conn ->
  withTransactionSerializable conn $ runBeamPostgresDebug putStrLn conn a

runDbQuiet :: MonadIO m => Pool Connection -> Pg a -> m a
runDbQuiet pool a = liftIO $ withResource pool $ \conn ->
  withTransactionSerializable conn $ runBeamPostgres conn a

-- | Run a serializable transaction and embed it in a larger context
runSerializable :: MonadIO m => Pool Connection -> Pg a -> m a
runSerializable connPool a = do
  let logger = putStrLn
  liftIO $ withResource connPool $ \dbConn -> liftIO $ withTransactionSerializable dbConn $
    runBeamPostgresDebug logger dbConn a
