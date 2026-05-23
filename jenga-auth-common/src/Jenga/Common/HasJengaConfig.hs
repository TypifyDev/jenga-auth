module Jenga.Common.HasJengaConfig
  ( HasConfig(..)
  , MkRoute
  , asksM
  , BaseURL(..)
  , DomainOption(..)
  , AuthCookieName(..)
  , UserTypeCookieName(..)
  , StripePlan(..)
  , CompanySignupCode(..)
  , SubscribeHash(..)
  , GithubOAuthClientID(..)
  , GithubOAuthClientSecret(..)
  , GoogleOAuthClientID(..)
  , GoogleOAuthClientSecret(..)
  , DiscordOAuthClientID(..)
  , DiscordOAuthClientSecret(..)
  , FreeTrialInfo(..)
  , FullRouteEncoder
  , IEncoder
  , StripeCode(..)
  , Plans(..)
  , getJsonConfigBase
  , getJsonConfig
  , renderFullRouteBE
  , renderFullRouteFE
  -- * Strong witness to the contained text being a valid link
  , Link(..)
  -- * Unwrap smart constructor
  , isLocalHostEnv
  , lookupSubscriptionCodeEnv
  , matchesCompanyCodeEnv

  )
where

import Data.Time.Clock
import qualified Control.Monad.Fail as Fail
import Data.Aeson
import qualified Data.Text as T
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Obelisk.Route as ObR
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Network.URI
import Control.Applicative
import GHC.Generics

class HasConfig a b where
  fromCfg :: a -> b

type MkRoute cfg be fe =
  ( HasConfig cfg (FullRouteEncoder be fe)
  , HasConfig cfg BaseURL
  )

asksM
  :: (HasConfig cfg x, Monad m) => ReaderT cfg m x
asksM = do
  r <- Control.Monad.Trans.Reader.ask
  pure $ fromCfg r

-- for type self-documentation
newtype BaseURL = BaseURL { getBaseURL :: URI }
newtype AuthCookieName = AuthCookieName { getAuthCookieName :: T.Text }
newtype UserTypeCookieName = UserTypeCookieName { getUserTypeCookieName :: T.Text }
newtype StripePlan = StripePlan { getStripePlan :: T.Text }
newtype CompanySignupCode = CompanySignupCode { getCompanySignupCode :: T.Text }
newtype SubscribeHash = SubscribeHash { getSubscribeHash :: T.Text } deriving (Eq, Ord)
newtype GithubOAuthClientID = GithubOAuthClientID { getGithubOAuthClientID :: T.Text } deriving (Eq, Ord)
newtype GithubOAuthClientSecret = GithubOAuthClientSecret { getGithubOAuthClientSecret :: T.Text } deriving (Eq, Ord)
newtype GoogleOAuthClientID = GoogleOAuthClientID { getGoogleOAuthClientID :: T.Text } deriving (Eq, Ord)
newtype GoogleOAuthClientSecret = GoogleOAuthClientSecret { getGoogleOAuthClientSecret :: T.Text } deriving (Eq, Ord)
newtype DiscordOAuthClientID = DiscordOAuthClientID { getDiscordOAuthClientID :: T.Text } deriving (Eq, Ord)
newtype DiscordOAuthClientSecret = DiscordOAuthClientSecret { getDiscordOAuthClientSecret :: T.Text } deriving (Eq, Ord)
data FreeTrialInfo = FreeTrialInfo
  { getFreeTrialCode :: T.Text
  , getFreeTrialLength :: NominalDiffTime
  } deriving (Eq, Ord)
type FullRouteEncoder be fe = IEncoder (ObR.R (FullRoute be fe)) PageName
type IEncoder a b = Encoder Identity Identity a b


getJsonConfigBase :: FromJSON a => T.Text -> Map.Map T.Text BS.ByteString -> (Maybe (Either String a))
getJsonConfigBase key cfgs = fmap eitherDecodeStrict' $ cfgs Map.!? key

-- | Get and parse a json configuration
getJsonConfig :: (FromJSON a, Fail.MonadFail m) => T.Text -> Map.Map T.Text BS.ByteString -> m a
getJsonConfig k cfgs = case getJsonConfigBase k cfgs of
  Nothing -> Fail.fail $ "getJsonConfig missing key: " <> T.unpack k
  Just (Left err) -> Fail.fail $ "getJsonConfig invalid for key " <> T.unpack k <> " : " <> err
  Just (Right val) -> pure val

renderFullRouteBE
  :: forall fe be cfg m.
     ( Monad m
     , HasConfig cfg (FullRouteEncoder be fe)
     , HasConfig cfg BaseURL
     )
  => ObR.R be
  -> ReaderT cfg m Link
renderFullRouteBE route = do
  (enc :: FullRouteEncoder be fe)  <- asksM -- _routeEncoder
  BaseURL baseUrl <- asksM
  pure . Link $ (T.pack $ show baseUrl) <> renderBackendRoute enc route

renderFullRouteFE
  :: forall be fe m cfg.
     ( Monad m
     , HasConfig cfg (FullRouteEncoder be fe)
     , HasConfig cfg BaseURL
     )
  => ObR.R fe
  -> ReaderT cfg m Link
renderFullRouteFE route = do
  (enc :: FullRouteEncoder be fe)  <- asksM -- _routeEncoder
  BaseURL baseUrl <- asksM
  pure . Link $ (T.pack $ show baseUrl) <> renderFrontendRoute enc route

-- Strong witness to the contained text being a valid link
newtype Link = Link { getLink :: T.Text } deriving Generic
instance ToJSON Link
instance FromJSON Link

isLocalHostEnv
  :: ( MonadIO m
     , HasConfig cfg (BaseURL)
     )
  => ReaderT cfg m Bool
isLocalHostEnv = T.isPrefixOf "http://localhost:" . T.pack . show . getBaseURL <$> asksM

data Plans = Plans
  { defaultPlan :: Maybe StripePlan
  , getPlans :: Map.Map SubscribeHash StripePlan
  }

lookupSubscriptionCodeEnv
  :: ( Monad m
     , HasConfig cfg Plans
     )
  => Maybe T.Text
  -> ReaderT cfg m (Maybe StripePlan)
lookupSubscriptionCodeEnv codeAsked = do
  plans <- asksM -- _subscriptionCodes (baseSubscription, codesMap)
  pure $ (codeAsked >>= flip Map.lookup (getPlans plans) . SubscribeHash) <|> defaultPlan plans

matchesCompanyCodeEnv
  :: ( Monad m
     , HasConfig cfg CompanySignupCode
     )
  => T.Text
  -> ReaderT cfg m Bool
matchesCompanyCodeEnv c = (==c) . getCompanySignupCode <$>  asksM

newtype StripeCode = StripeCode T.Text

data DomainOption = ProxiedDomain T.Text T.Text | DirectDomain T.Text
