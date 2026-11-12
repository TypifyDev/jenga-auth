-- | Umbrella re-export. Every Jenga beam-table lives in its own
-- per-table module under 'Jenga.Common.Schema.*'; this module
-- exposes them all transparently so legacy consumers can keep
-- @import Jenga.Common.Schema@.
--
-- NEW consumers should import the per-table modules directly
-- (e.g. @import Jenga.Common.Schema.SendEmailTask@) — the umbrella
-- stays as a backward-compat shim.
module Jenga.Common.Schema
  ( module Jenga.Common.Schema.SendEmailTask
  , module Jenga.Common.Schema.StripeRelation
  , module Jenga.Common.Schema.Unsubscribe
  , module Jenga.Common.Schema.UserTypeTable
  , module Jenga.Common.Schema.OrgOwnedUsers
  , module Jenga.Common.Schema.LogItemRow
  , module Jenga.Common.Schema.InviteLink
  , module Jenga.Common.Schema.FreeTrial
  , module Jenga.Common.Schema.GithubID
  ) where

import Jenga.Common.Schema.SendEmailTask
import Jenga.Common.Schema.StripeRelation
import Jenga.Common.Schema.Unsubscribe
import Jenga.Common.Schema.UserTypeTable
import Jenga.Common.Schema.OrgOwnedUsers
import Jenga.Common.Schema.LogItemRow
import Jenga.Common.Schema.InviteLink
import Jenga.Common.Schema.FreeTrial
import Jenga.Common.Schema.GithubID
