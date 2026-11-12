{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-deriving-defaults #-}
module Jenga.Common.Schema.InviteLink where

import Data.Aeson
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Kind (Constraint)
import qualified Data.Text as T
import Database.Beam.Backend.SQL.Types
import Database.Beam.Schema
import GHC.Generics
import Jenga.Common.Company (CompanyInfo)

data InviteLink f = InviteLink
  { _inviteLink_companyId :: PrimaryKey CompanyInfo f
  , _inviteLink_code :: Columnar f T.Text
  , _inviteLink_numLeft :: Columnar f (Maybe Int64) -- Org may set a cap on how many can use their link
  } deriving Generic

instance Table InviteLink where
  data PrimaryKey InviteLink f = InviteCode { get_inviteCode :: Columnar f T.Text } deriving Generic
  primaryKey = InviteCode <$> _inviteLink_code

instance Beamable InviteLink
instance Beamable (PrimaryKey InviteLink)

type HasInviteLinkTableConstraint (c :: * -> Constraint) f =
  ( c (Columnar f (SqlSerial Int64))
  , c (Columnar f T.Text)
  , c (Columnar f (Maybe Int64))
  , c (PrimaryKey CompanyInfo f)
  )

type HasInviteLinkTableIdConstraint (c :: * -> Constraint) f =
  ( c (Columnar f T.Text)
  )

deriving instance Show (PrimaryKey InviteLink Identity)
deriving instance Eq (PrimaryKey InviteLink Identity)
instance ToJSON (PrimaryKey InviteLink Identity)
instance FromJSON (PrimaryKey InviteLink Identity)
instance ToJSONKey (PrimaryKey InviteLink Identity)
instance FromJSONKey (PrimaryKey InviteLink Identity)
