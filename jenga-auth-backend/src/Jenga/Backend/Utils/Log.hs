module Jenga.Backend.Utils.Log where

import Jenga.Common.Log
import Jenga.Common.Schema
import Jenga.Backend.Utils.Email
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable

import Rhyolite.DB.Beam
import Database.Beam.Query
import Database.Beam.Postgres

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Exception
import Data.Pool
import qualified Data.Text as T
import Prelude hiding (log)


reportError
  :: forall db cfg be n a m.
    ( MonadIO m
    , Show a
    , HasJengaTable Postgres db SendEmailTask
    , HasConfig cfg AdminEmail
    , HasConfig cfg (Pool Connection)
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => a
  -> ReaderT cfg m ()
reportError = newAdminEmail @db "Error on production" . T.pack . show

reportOnError
  :: forall db cfg be n m a b.
    ( MonadIO m
    , Show a
    , HasJengaTable Postgres db SendEmailTask
    , HasConfig cfg AdminEmail
    , HasConfig cfg (Pool Connection)
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => Either a b -> ReaderT cfg m ()
reportOnError = \case
  Left err -> reportError @db err
  Right _ -> pure ()

catchToss :: IO a -> IO ()
catchToss m = catch (m >> pure ()) (\(_ :: IOException) -> pure ())

reportLog
  :: forall db cfg m log.
     ( Loggable log
     , MonadIO m
     , HasJengaTable Postgres db LogItemRow
     , HasConfig cfg (Pool Connection)
     )
  => Bool
  -> log
  -> ReaderT cfg m ()
reportLog isUrgent item = do
  (logTbl :: PgTable Postgres db LogItemRow) <- asksTableM
  withDbEnvQuiet $ do
    runInsert $ insert logTbl $ insertExpressions
      [ LogItemRow
        default_
        (val_ False)
        current_timestamp_
        (val_ isUrgent)
        (val_ $ renderLog item)
      ]
