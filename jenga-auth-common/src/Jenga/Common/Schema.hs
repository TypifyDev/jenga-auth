{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

--{-# Language DeriveAnyClass #-}
--{-# Language StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-deriving-defaults #-}
module Jenga.Common.Schema where



import Jenga.Common.Auth
import Jenga.Common.Company
import Rhyolite.Account (Account)
import Control.Lens.TH
import Data.Aeson
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Text
import Database.Beam.Backend.SQL.Types
import Database.Beam.Schema
import GHC.Generics
import Network.Mail.Mime (Mail)
import Network.Mail.Mime.Orphans ()
import qualified Data.Text as T
import Data.Kind (Constraint)
import Data.Time

data SendEmailTask f = SendEmailTask
  { _sendEmailTask_id :: Columnar f (SqlSerial Int64)
  , _sendEmailTask_isUrgent :: Columnar f (Maybe Bool)
  , _sendEmailTask_finished :: Columnar f Bool
  , _sendEmailTask_checkedOutBy :: Columnar f (Maybe Text)
  , _sendEmailTask_payload :: Columnar f Mail
  , _sendEmailTask_result :: Columnar f (Maybe Bool)
  } deriving (Generic)-- Beamable)

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


data StripeRelation f = StripeRelation
  { _stripeRelation_uid :: Columnar f Int64
  , _stripeRelation_customerId :: Columnar f T.Text -- TODO: use  the stripe-core types
  , _stripeRelation_subscriptionId :: Columnar f (Maybe T.Text)
  --, _stripeRelation_freeTrialStart :: Columnar f (Maybe T.Text)
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

data UserTypeTable f = UserTypeTable
  { _userType_acctID :: Columnar f Int64
  , _userType_userType :: Columnar f UserType
  , _userType_companyID :: PrimaryKey CompanyInfo (Nullable f)
  -- ^ If type == Admin then companyID must be (Just _)
  } deriving Generic

instance Table UserTypeTable where
  data PrimaryKey UserTypeTable f = UTypeID
    { get_acctID :: Columnar f Int64
    -- , get_companyID :: Columnar f (Maybe T.Text)
    } deriving Generic
  primaryKey = UTypeID <$> _userType_acctID -- <*> _userType_companyID

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


data GithubID f = GithubID
  { _githubID_ghid :: Columnar f Int64
  , _githubID_userID :: Columnar f Int64 -- I think it would be better if this were ShortByteString
  } deriving Generic

instance Beamable GithubID

instance Table GithubID where
  newtype PrimaryKey GithubID f = GitID { unGithubIDId :: Columnar f Int64 }
    deriving Generic
  primaryKey = GitID <$> _githubID_ghid

type HasGithubIDConstraint (c :: * -> Constraint) f =
  ( c (Columnar f Int64)
  , c (Columnar f Text)
  )

type HasGithubIDIdConstraint (c :: * -> Constraint) f =
  ( c (Columnar f Int64)
  )

deriving instance HasGithubIDConstraint Eq f => Eq (GithubID f)
deriving instance HasGithubIDConstraint Ord f => Ord (GithubID f)
deriving instance HasGithubIDConstraint Show f => Show (GithubID f)

deriving instance HasGithubIDIdConstraint Eq f => Eq (PrimaryKey GithubID f)
deriving instance HasGithubIDIdConstraint Ord f => Ord (PrimaryKey GithubID f)
deriving instance HasGithubIDIdConstraint Read f => Read (PrimaryKey GithubID f)
deriving instance HasGithubIDIdConstraint Show f => Show (PrimaryKey GithubID f)
deriving instance HasGithubIDIdConstraint ToJSON f => ToJSON (PrimaryKey GithubID f)
deriving instance HasGithubIDIdConstraint ToJSONKey f => ToJSONKey (PrimaryKey GithubID f)
deriving instance HasGithubIDIdConstraint FromJSON f => FromJSON (PrimaryKey GithubID f)
deriving instance HasGithubIDIdConstraint FromJSONKey f => FromJSONKey (PrimaryKey GithubID f)

deriving instance HasGithubIDIdConstraint Semigroup f => Semigroup (PrimaryKey GithubID f)
deriving instance HasGithubIDIdConstraint Monoid f => Monoid (PrimaryKey GithubID f)

instance Beamable (PrimaryKey GithubID)



-- | The data we care about that github provides us during the token exchange process
data GithubAccessTokenResponse = GithubAccessTokenResponse
  { access_token :: T.Text
  , scope :: T.Text
  , token_type :: T.Text
  }
  deriving (Generic, Eq)

instance FromJSON GithubAccessTokenResponse

-- GPT
data GitHubUser = GitHubUser
  { _githubUser_login :: String
  , _githubUser_github_id :: Int
  , _githubUser_node_id :: String
  , _githubUser_avatar_url :: String
  , _githubUser_gravatar_id :: String
  , _githubUser_url :: String
  , _githubUser_html_url :: String
  , _githubUser_followers_url :: String
  , _githubUser_following_url :: String
  , _githubUser_gists_url :: String
  , _githubUser_starred_url :: String
  , _githubUser_subscriptions_url :: String
  , _githubUser_organizations_url :: String
  , _githubUser_repos_url :: String
  , _githubUser_events_url :: String
  , _githubUser_received_events_url :: String
  , _githubUser_user_type :: String
  , _githubUser_site_admin :: Bool
  , _githubUser_name :: Maybe String
  , _githubUser_company :: Maybe String
  , _githubUser_blog :: Maybe String
  , _githubUser_location :: Maybe String
  , _githubUser_github_email :: Maybe String
  , _githubUser_hireable :: Maybe Bool
  , _githubUser_bio :: Maybe String
  , _githubUser_twitter_username :: Maybe String
  , _githubUser_public_repos :: Int
  , _githubUser_public_gists :: Int
  , _githubUser_followers :: Int
  , _githubUser_following :: Int
  , _githubUser_created_at :: String
  , _githubUser_updated_at :: String
  } deriving (Show, Generic)

-- GPT: Define an instance of FromJSON for the GitHubUser type
instance FromJSON GitHubUser where
  parseJSON = withObject "GitHubUser" $ \v ->
    GitHubUser
      <$> v .: "login"
      <*> v .: "id"
      <*> v .: "node_id"
      <*> v .: "avatar_url"
      <*> v .: "gravatar_id"
      <*> v .: "url"
      <*> v .: "html_url"
      <*> v .: "followers_url"
      <*> v .: "following_url"
      <*> v .: "gists_url"
      <*> v .: "starred_url"
      <*> v .: "subscriptions_url"
      <*> v .: "organizations_url"
      <*> v .: "repos_url"
      <*> v .: "events_url"
      <*> v .: "received_events_url"
      <*> v .: "type"
      <*> v .: "site_admin"
      <*> v .:? "name"
      <*> v .:? "company"
      <*> v .:? "blog"
      <*> v .:? "location"
      <*> v .:? "email"
      <*> v .:? "hireable"
      <*> v .:? "bio"
      <*> v .:? "twitter_username"
      <*> v .: "public_repos"
      <*> v .: "public_gists"
      <*> v .: "followers"
      <*> v .: "following"
      <*> v .: "created_at"
      <*> v .: "updated_at"

data Authd = GithubAuthd GithubIDRequest --  | other auths eg. BasicAuth
data GithubIDRequest = GithubIDRequest T.Text


makeLenses ''SendEmailTask
