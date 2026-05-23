{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Jenga.Common.Company where

import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Data.Aeson
import Data.Int (Int64)
import qualified Data.Text as T

data CompanyInfo f = CompanyInfo
  { _companyInfo_id       :: Columnar f (SqlSerial Int64)
  , _companyInfo_name     :: Columnar f T.Text
  , _companyInfo_industry :: Columnar f (Maybe T.Text)
  , _companyInfo_website  :: Columnar f (Maybe T.Text)
  , _companyInfo_size     :: Columnar f (Maybe T.Text)
  , _companyInfo_about    :: Columnar f (Maybe T.Text)
  } deriving Generic

instance Beamable CompanyInfo
instance Beamable (PrimaryKey CompanyInfo)

instance Table CompanyInfo where
  data PrimaryKey CompanyInfo f = CompanyInfoId
    { _companyInfoId :: Columnar f (SqlSerial Int64)
    } deriving Generic
  primaryKey = CompanyInfoId <$> _companyInfo_id

deriving instance Show (CompanyInfo Identity)
deriving instance Show (PrimaryKey CompanyInfo Identity)
deriving instance Eq (PrimaryKey CompanyInfo Identity)
deriving instance Ord (PrimaryKey CompanyInfo Identity)
instance ToJSON (CompanyInfo Identity)
instance FromJSON (CompanyInfo Identity)
instance ToJSON (PrimaryKey CompanyInfo Identity)
instance FromJSON (PrimaryKey CompanyInfo Identity)
instance ToJSONKey (PrimaryKey CompanyInfo Identity)
instance FromJSONKey (PrimaryKey CompanyInfo Identity)
