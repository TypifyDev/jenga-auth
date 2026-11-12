{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-deriving-defaults #-}
module Jenga.Common.Schema.SendEmailTask where

import Control.Lens.TH
import Data.Aeson
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Text (Text)
import Database.Beam.Backend.SQL.Types
import Database.Beam.Schema
import GHC.Generics
import Network.Mail.Mime (Mail)
import Network.Mail.Mime.Orphans ()

data SendEmailTask f = SendEmailTask
  { _sendEmailTask_id :: Columnar f (SqlSerial Int64)
  , _sendEmailTask_isUrgent :: Columnar f (Maybe Bool)
  , _sendEmailTask_finished :: Columnar f Bool
  , _sendEmailTask_checkedOutBy :: Columnar f (Maybe Text)
  , _sendEmailTask_payload :: Columnar f Mail
  , _sendEmailTask_result :: Columnar f (Maybe Bool)
  } deriving (Generic)

instance Beamable SendEmailTask
instance Beamable (PrimaryKey SendEmailTask)

instance Table SendEmailTask where
  newtype PrimaryKey SendEmailTask f = SendEmailTaskId
    { unSendEmailTaskId :: Columnar f (SqlSerial Int64)
    } deriving (Generic)
  primaryKey = SendEmailTaskId . _sendEmailTask_id

deriving instance Show (PrimaryKey SendEmailTask Identity)
deriving instance Eq (PrimaryKey SendEmailTask Identity)
instance ToJSON (PrimaryKey SendEmailTask Identity)
instance FromJSON (PrimaryKey SendEmailTask Identity)
instance ToJSONKey (PrimaryKey SendEmailTask Identity)
instance FromJSONKey (PrimaryKey SendEmailTask Identity)

makeLenses ''SendEmailTask
