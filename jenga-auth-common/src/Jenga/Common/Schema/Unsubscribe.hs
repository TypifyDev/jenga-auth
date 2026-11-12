{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-deriving-defaults #-}
module Jenga.Common.Schema.Unsubscribe where

import Data.Aeson
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Kind (Constraint)
import Data.Text (Text)
import Database.Beam.Backend.SQL.Types
import Database.Beam.Schema
import GHC.Generics

data Unsubscribe f = Unsubscribe
  { _unsubscribe_uid :: Columnar f (SqlSerial Int64) -- Nothing if its a promotional link by us
  , _unsubscribe_email :: Columnar f Text -- Org may set a cap on how many can use their link
  } deriving Generic

instance Table Unsubscribe where
  data PrimaryKey Unsubscribe f = Unsub { _unsub_uid :: Columnar f (SqlSerial Int64) } deriving Generic
  primaryKey = Unsub <$> _unsubscribe_uid

instance Beamable Unsubscribe
instance Beamable (PrimaryKey Unsubscribe)

type HasUnsubscribeTableConstraint (c :: * -> Constraint) f =
  ( c (Columnar f (SqlSerial Int64))
  , c (Columnar f Text)
  )

type HasUnsubscribeTableIdConstraint (c :: * -> Constraint) f =
  ( c (Columnar f (SqlSerial Int64))
  )

deriving instance Show (PrimaryKey Unsubscribe Identity)
deriving instance Eq (PrimaryKey Unsubscribe Identity)
instance ToJSON (PrimaryKey Unsubscribe Identity)
instance FromJSON (PrimaryKey Unsubscribe Identity)
instance ToJSONKey (PrimaryKey Unsubscribe Identity)
instance FromJSONKey (PrimaryKey Unsubscribe Identity)
