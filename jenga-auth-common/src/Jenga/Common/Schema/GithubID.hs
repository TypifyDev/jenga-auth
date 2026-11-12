{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-deriving-defaults #-}
module Jenga.Common.Schema.GithubID where

import Data.Aeson
import Data.Int (Int64)
import Data.Kind (Constraint)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam.Schema
import GHC.Generics

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
