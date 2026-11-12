{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-deriving-defaults #-}
module Jenga.Common.Schema.FreeTrial where

import Data.Aeson
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Kind (Constraint)
import qualified Data.Text as T
import Data.Time
import Database.Beam.Schema
import GHC.Generics

data FreeTrial f = FreeTrial
  { _freeTrial_userID :: Columnar f Int64
  , _freeTrial_startDate :: Columnar f UTCTime
  , _freeTrial_discountCode :: Columnar f (Maybe T.Text)
  } deriving Generic

instance Beamable FreeTrial
instance Beamable (PrimaryKey FreeTrial)
instance Table FreeTrial where
  newtype PrimaryKey FreeTrial f = FreeTrial_UID { unFreeTrialUID :: Columnar f Int64 } deriving Generic
  primaryKey = FreeTrial_UID <$> _freeTrial_userID

type HasFreeTrialConstraint (c :: * -> Constraint) f =
  ( c (Columnar f Int64)
  , c (Columnar f UTCTime)
  , c (Columnar f T.Text)
  )

type HasFreeTrialIdConstraint (c :: * -> Constraint) f =
  ( c (Columnar f Int64)
  )

deriving instance Show (PrimaryKey FreeTrial Identity)
deriving instance Eq (PrimaryKey FreeTrial Identity)
instance ToJSON (PrimaryKey FreeTrial Identity)
instance FromJSON (PrimaryKey FreeTrial Identity)
instance ToJSONKey (PrimaryKey FreeTrial Identity)
instance FromJSONKey (PrimaryKey FreeTrial Identity)

type DiscountCode = T.Text
