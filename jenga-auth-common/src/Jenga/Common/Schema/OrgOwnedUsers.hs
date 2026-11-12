{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-deriving-defaults #-}
module Jenga.Common.Schema.OrgOwnedUsers where

import Data.Aeson
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Kind (Constraint)
import Database.Beam.Backend.SQL.Types
import Database.Beam.Schema
import GHC.Generics
import Jenga.Common.Company (CompanyInfo)
import Rhyolite.Account (Account)

-- | Maps an account to a company they belong to as a managed user.
-- The account must exist before this row is created.
data OrgOwnedUsers f = OrgOwnedUsers
  { _orgOwnedUsers_accountId :: PrimaryKey Account f
  , _orgOwnedUsers_companyId :: PrimaryKey CompanyInfo f
  } deriving Generic

instance Beamable OrgOwnedUsers
instance Beamable (PrimaryKey OrgOwnedUsers)

type HasOrgOwnedUsersConstraint (c :: * -> Constraint) f =
  ( c (Columnar f (SqlSerial Int64))
  , c (PrimaryKey Account f)
  , c (PrimaryKey CompanyInfo f)
  )

instance Table OrgOwnedUsers where
  data PrimaryKey OrgOwnedUsers f = OrgOwnedUserId
    { _orgOwnedUserId :: PrimaryKey Account f
    } deriving Generic
  primaryKey = OrgOwnedUserId <$> _orgOwnedUsers_accountId

deriving instance HasOrgOwnedUsersConstraint Eq f => Eq (OrgOwnedUsers f)
deriving instance HasOrgOwnedUsersConstraint Show f => Show (OrgOwnedUsers f)

deriving instance Eq (PrimaryKey OrgOwnedUsers Identity)
deriving instance Show (PrimaryKey OrgOwnedUsers Identity)
instance ToJSON (PrimaryKey OrgOwnedUsers Identity)
instance FromJSON (PrimaryKey OrgOwnedUsers Identity)
instance ToJSON (OrgOwnedUsers Identity)
instance FromJSON (OrgOwnedUsers Identity)
