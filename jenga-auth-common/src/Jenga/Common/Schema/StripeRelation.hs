{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-deriving-defaults #-}
module Jenga.Common.Schema.StripeRelation where

import Data.Aeson
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Kind (Constraint)
import qualified Data.Text as T
import Database.Beam.Schema
import GHC.Generics

data StripeRelation f = StripeRelation
  { _stripeRelation_uid :: Columnar f Int64
  , _stripeRelation_customerId :: Columnar f T.Text -- TODO: use the stripe-core types
  , _stripeRelation_subscriptionId :: Columnar f (Maybe T.Text)
  } deriving Generic

instance Beamable StripeRelation
instance Beamable (PrimaryKey StripeRelation)
instance Table StripeRelation where
  newtype PrimaryKey StripeRelation f = StripeKey_UID { unStripeUID :: Columnar f Int64 } deriving Generic
  primaryKey = StripeKey_UID <$> _stripeRelation_uid

type HasStripeRelationConstraint (c :: * -> Constraint) f =
  ( c (Columnar f Int64)
  , c (Columnar f T.Text)
  , c (Columnar f T.Text)
  , c (Columnar f T.Text)
  )

type HasStripeRelationIdConstraint (c :: * -> Constraint) f =
  ( c (Columnar f Int64)
  )

deriving instance Show (PrimaryKey StripeRelation Identity)
deriving instance Eq (PrimaryKey StripeRelation Identity)
instance ToJSON (PrimaryKey StripeRelation Identity)
instance FromJSON (PrimaryKey StripeRelation Identity)
instance ToJSONKey (PrimaryKey StripeRelation Identity)
instance FromJSONKey (PrimaryKey StripeRelation Identity)
