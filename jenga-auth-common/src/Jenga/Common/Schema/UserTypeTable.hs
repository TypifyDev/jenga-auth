{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-deriving-defaults #-}
module Jenga.Common.Schema.UserTypeTable where

import Data.Aeson
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Kind (Constraint)
import Database.Beam.Schema
import GHC.Generics
import Jenga.Common.Auth (UserType)
import Jenga.Common.Company (CompanyInfo)

data UserTypeTable f = UserTypeTable
  { _userType_acctID :: Columnar f Int64
  , _userType_userType :: Columnar f UserType
  , _userType_companyID :: PrimaryKey CompanyInfo (Nullable f)
  -- ^ If type == Admin then companyID must be (Just _)
  } deriving Generic

instance Table UserTypeTable where
  data PrimaryKey UserTypeTable f = UTypeID
    { get_acctID :: Columnar f Int64
    } deriving Generic
  primaryKey = UTypeID <$> _userType_acctID

instance Beamable UserTypeTable
instance Beamable (PrimaryKey UserTypeTable)

type HasUserTypeTableConstraint (c :: * -> Constraint) f =
  ( c (Columnar f Int64)
  , c (Columnar f UserType)
  )

type HasUserTypeTableIdConstraint (c :: * -> Constraint) f =
  ( c (Columnar f Int64)
  )

deriving instance Show (PrimaryKey UserTypeTable Identity)
deriving instance Eq (PrimaryKey UserTypeTable Identity)
instance ToJSON (PrimaryKey UserTypeTable Identity)
instance FromJSON (PrimaryKey UserTypeTable Identity)
instance ToJSONKey (PrimaryKey UserTypeTable Identity)
instance FromJSONKey (PrimaryKey UserTypeTable Identity)
