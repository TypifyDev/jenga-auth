{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-deriving-defaults #-}
module Jenga.Common.Schema.LogItemRow where

import Data.Aeson
import Data.Int (Int64)
import Data.Kind (Constraint)
import Data.Text (Text)
import Data.Time
import Database.Beam.Backend.SQL.Types
import Database.Beam.Schema
import GHC.Generics

data LogItemRow f = LogItemRow
  { _logItemRow_id :: Columnar f (SqlSerial Int64)
  , _logItemRow_hasBeenSent :: Columnar f Bool
  , _logItemRow_insertionTime :: Columnar f UTCTime
  , _logItemRow_isUrgent :: Columnar f Bool
  , _logItemRow_payload :: Columnar f Text
  } deriving Generic

instance Beamable LogItemRow

type HasLogItemRowConstraint (c :: * -> Constraint) f =
  ( c (Columnar f (SqlSerial Int64))
  , c (Columnar f Bool)
  , c (Columnar f UTCTime)
  , c (Columnar f Bool)
  , c (Columnar f Text)
  )

type HasLogItemRowIdConstraint (c :: * -> Constraint) f =
  ( c (Columnar f (SqlSerial Int64))
  )

deriving instance HasLogItemRowConstraint Eq f => Eq (LogItemRow f)
deriving instance HasLogItemRowConstraint Ord f => Ord (LogItemRow f)
deriving instance HasLogItemRowConstraint Show f => Show (LogItemRow f)

deriving instance HasLogItemRowIdConstraint Eq f => Eq (PrimaryKey LogItemRow f)
deriving instance HasLogItemRowIdConstraint Ord f => Ord (PrimaryKey LogItemRow f)
deriving instance HasLogItemRowIdConstraint Read f => Read (PrimaryKey LogItemRow f)
deriving instance HasLogItemRowIdConstraint Show f => Show (PrimaryKey LogItemRow f)
deriving instance HasLogItemRowIdConstraint ToJSON f => ToJSON (PrimaryKey LogItemRow f)
deriving instance HasLogItemRowIdConstraint ToJSONKey f => ToJSONKey (PrimaryKey LogItemRow f)
deriving instance HasLogItemRowIdConstraint FromJSON f => FromJSON (PrimaryKey LogItemRow f)
deriving instance HasLogItemRowIdConstraint FromJSONKey f => FromJSONKey (PrimaryKey LogItemRow f)

deriving instance HasLogItemRowIdConstraint Semigroup f => Semigroup (PrimaryKey LogItemRow f)
deriving instance HasLogItemRowIdConstraint Monoid f => Monoid (PrimaryKey LogItemRow f)

instance Beamable (PrimaryKey LogItemRow)
instance Table LogItemRow where
  newtype PrimaryKey LogItemRow f = LogItemRow_Key { logKey :: Columnar f (SqlSerial Int64) } deriving Generic
  primaryKey = LogItemRow_Key <$> _logItemRow_id
