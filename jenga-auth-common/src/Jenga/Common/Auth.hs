{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Jenga.Common.Auth where

import Jenga.Common.Errors
import Jenga.Common.Company
import Jenga.Common.BeamExtras

import Web.Stripe.Error
import Rhyolite.Account
import Database.Beam
#ifndef ghcjs_HOST_OS
import Data.Text.Normalize
#endif
import Text.Email.Validate
import qualified Data.Text as T
import Data.Signed
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.String

type AuthToken = Signed (Id Account)

clientTypeHeader :: IsString s => s
clientTypeHeader = "X-ClientType"
data ClientType = Mobile | Web deriving (Show, Read)

-- | Transient type for ensuring necessary relations exist on creating an Account
data IsUserType
  = IsCompany (PrimaryKey CompanyInfo Identity)
  | IsGroupUser EmailAddress (PrimaryKey CompanyInfo Identity)
  | IsSelf
  -- most of the time, this means they are a direct subscriber
  -- or the service is free


type OrgName = T.Text

-- | Used for password reset
newtype Email = Email { unEmail :: T.Text } deriving (Show, Eq, Ord, Generic)
instance ToJSON Email
instance FromJSON Email

newtype Username = Username { getUsername :: T.Text } deriving (Show, Eq)

data NewUserEmail = NewUserEmail
  { newUserEmail_email :: EmailAddress
  , newUserEmail_code :: T.Text
  } deriving (Show,Generic)

data NewCompanyEmail = NewCompanyEmail
  { newCompanyEmail_email :: EmailAddress -- T.Text
  , newCompanyEmail_orgName :: OrgName
  , newCompanyEmail_code :: T.Text
  } deriving (Show,Generic)

data Password = Password { unPassword :: T.Text } deriving Generic

toText :: Password -> T.Text
toText = unPassword

fromText :: T.Text -> Password
fromText = Password
#ifndef ghcjs_HOST_OS
  . normalize NFKC
#endif



instance ToJSON Password where
  toJSON = toJSON . toText
instance FromJSON Password where
  parseJSON = fmap fromText . parseJSON



type ErrorRequirements err = (Show err, ShowUser err, ToJSON err, FromJSON err, Typeable err)

instance SpecificError (BackendError ())
instance SpecificError (BackendError LoginError)
instance SpecificError (BackendError ResetPasswordError)
instance SpecificError (BackendError RequestPasswordResetError)
instance SpecificError (BackendError AddUsersError)
instance SpecificError (BackendError FreeTrialError)
instance SpecificError (BackendError RedeemLinkError)
instance SpecificError (BackendError SubscribeError)
instance SpecificError (BackendError CancelSubError)
instance SpecificError (BackendError NoFreeTrialCode)
instance SpecificError (BackendError AdminSignupError)
instance SpecificError (BackendError InviteError)
instance SpecificError (BackendError UnsubscribeError)
instance SpecificError (BackendError OAuthError)
instance SpecificError (BackendError UserSignupError)

-- | TODO: we currently have zero actual support for creating a
-- | super user, however it is here to ensure future support.
-- | such as a handler which requires this account ID is the super user
data UserType = Self | Admin | SuperUser deriving (Eq, Show, Read, Generic)
instance FromJSON UserType
instance ToJSON UserType

-- New idea:
data Self' = Self' deriving (Eq, Show, Read, Generic)
data Admin' = Admin' | OwnedS Self' deriving (Eq, Show, Read, Generic)
data SuperUser' = SuperUser' | OwnedA Admin' deriving (Eq, Show, Read, Generic)

-- g :: UserType t => t -> m a

f1 :: SuperUser' -> Int
f1 = \case
  SuperUser' -> 0
  OwnedA user -> case user of
    Admin' -> 1
    OwnedS Self' -> 2
--OR
f2 :: Admin' -> Int
f2 = \case
  Admin' -> 0
  OwnedS Self' -> 1
-- OR
f3 :: Self' -> Int
f3 = \case
  Self' -> 0


data FreeTrialError
  = InvalidEmail_FreeTrial
  | FreeTrial_Signup UserSignupError
  | NoUserForFreeTrial
  deriving (Show, Eq, Generic)
instance ToJSON FreeTrialError
instance FromJSON FreeTrialError
instance ShowUser FreeTrialError where
  showUser InvalidEmail_FreeTrial = "Invalid email"
  showUser (FreeTrial_Signup serr) = "When creating account: " <> showUser serr
  showUser NoUserForFreeTrial = "Error creating user: on get user: not found"

data RedeemLinkError
  = NoOrgCode_RedeemLink
  | CreditsDepleted T.Text
  | InvalidEmail_RedeemLink
  | RedeemLink_Signup UserSignupError
  deriving (Show, Eq, Generic)
instance ToJSON RedeemLinkError
instance FromJSON RedeemLinkError
instance ShowUser RedeemLinkError where
  showUser NoOrgCode_RedeemLink = "No organization code found"
  showUser (CreditsDepleted orgName) = "No more credits available for " <> orgName <> ", please consult with link provider"
  showUser InvalidEmail_RedeemLink = "Invalid email"
  showUser (RedeemLink_Signup serr) = "While redeeming link: " <> showUser serr

data AddUserOutcome
  = AddUser_Added T.Text
  | AddUser_Skipped T.Text
  | AddUser_Failed T.Text
  deriving (Show, Eq, Generic)
instance ToJSON AddUserOutcome
instance FromJSON AddUserOutcome

data AddUsersResult = AddUsersResult
  { _addUsersResult_added :: Int
  , _addUsersResult_skipped :: [T.Text]
  , _addUsersResult_failed :: [T.Text]
  } deriving (Show, Eq, Generic)
instance ToJSON AddUsersResult
instance FromJSON AddUsersResult

data AddUsersError
  = NoCommas
  | NoUsersGiven
  | InvalidEmail_AddUser [T.Text]
  | NoOrgCode T.Text
  | AddUser_Signup UserSignupError
  deriving (Show, Eq, Generic)
instance ToJSON AddUsersError
instance FromJSON AddUsersError
instance ShowUser AddUsersError where
  showUser NoCommas = "Error reading list, please ensure all emails are separated by commas or newlines"
  showUser NoUsersGiven = "No users given"
  showUser (InvalidEmail_AddUser badEmails) = "Invalid emails: " <> T.intercalate ", " badEmails
  showUser (NoOrgCode _) = "No organization code found"
  showUser (AddUser_Signup serr) = showUser serr

-- TODO: if we use bad email as an error elsewhere, turn this into a type declaration
data InviteError
  = BadEmail
  | InviteSignupFailed UserSignupError
  deriving (Eq, Show,Generic)
instance ToJSON InviteError
instance FromJSON InviteError
instance ShowUser InviteError where
  showUser BadEmail = "Invalid email: the supplied email must be valid"
  showUser (InviteSignupFailed signupErr) = "Couldn't create account for invited user: " <> showUser signupErr

data UserSignupError
  = FailedMakeResetToken
  | AccountExists
  | BadSignupEmail
  | NoEmailSent
  | NoLinkedOrganization
  | AccountInsertFailed Int
  deriving (Eq,Show,Generic)
instance ToJSON UserSignupError
instance FromJSON UserSignupError
instance ShowUser UserSignupError where
  showUser FailedMakeResetToken = "Couldn't create one-time token for account setup"
  showUser AccountExists = "Account already exists"
  showUser BadSignupEmail = "Please provide a valid email"
  showUser NoEmailSent = "We could not send email"
  showUser NoLinkedOrganization = "You seem to be joining under an organization that doesn't exist"
  showUser (AccountInsertFailed n) = "Account creation failed: insert returned " <> T.pack (show n) <> " rows instead of 1"

data UnsubscribeError = EmailNotFound deriving (Show,Eq,Generic)
instance FromJSON UnsubscribeError
instance ToJSON UnsubscribeError
instance ShowUser UnsubscribeError where
  showUser EmailNotFound = "Email not found, could not unsubscribe"

data ResetPasswordError
  = InvalidToken
  | Unknown T.Text
  | CouldntRetrieveAccount
  | Reset_NoUserTypeFound
  | Reset_NoEmailSent
  deriving (Eq,Show,Generic)
instance FromJSON ResetPasswordError
instance ToJSON ResetPasswordError
instance ShowUser ResetPasswordError where
  showUser InvalidToken = "Invalid token, this link has probably expired, please request a new one"
  showUser (Unknown err) =  "Unknown Error on reset password" <> err
  showUser CouldntRetrieveAccount = "Could not find account when trying to reset password, this error has been reported"
  showUser Reset_NoUserTypeFound = "No user type found"
  showUser Reset_NoEmailSent = "We were unable to send you an email"

data RequestPasswordResetError
  = InvalidEmail
  | NotSignedUp
  | FailedMakeNonce
  deriving (Eq,Show,Generic)
instance FromJSON RequestPasswordResetError
instance ToJSON RequestPasswordResetError
instance ShowUser RequestPasswordResetError where
  showUser InvalidEmail = "Invalid Email"
  showUser NotSignedUp = "An account with that email does not exist, please signup first"
  showUser FailedMakeNonce = "Failed to make one-time link"

data SubscribeError
  = MissingEmail
  | StripeGenError StripeError -- renders with `errorMsg`
  | NoAssociatedStripeInfo
  | AlreadyInGroupSubscription
  | AlreadyInSelfSubscription
  | Stripe_FailedMkNonce
  | SubFailedEmail T.Text
  | NoMatchingPlanFound
  deriving (Show,Generic)
instance ToJSON StripeError
deriving instance Generic StripeError
instance ToJSON StripeErrorHTTPCode
instance ToJSON StripeErrorCode
instance ToJSON StripeErrorType
deriving instance Generic StripeErrorHTTPCode
deriving instance Generic StripeErrorCode
deriving instance Generic StripeErrorType
instance FromJSON SubscribeError
instance ToJSON SubscribeError
instance ShowUser SubscribeError where
  showUser MissingEmail = "Email not found"
  showUser (StripeGenError stripeErr) = "Payment returned error: " <> errorMsg stripeErr
  showUser NoAssociatedStripeInfo = "Your account exists but no stripe info was found, this issue has been reported"
  showUser AlreadyInGroupSubscription = "Your account already exists under a group agreement"
  showUser AlreadyInSelfSubscription = "You already have a subscription for this email"
  showUser Stripe_FailedMkNonce = "Nonce Error: Failed to make nonce"
  showUser (SubFailedEmail _) = "Unable to send email, we will manually send"
  showUser NoMatchingPlanFound = "Unable to find valid plan for this code. No free version exists"

data NoFreeTrialCode = NoFreeTrialCode deriving (Eq,Show,Generic)
instance FromJSON NoFreeTrialCode
instance ToJSON NoFreeTrialCode
instance ShowUser NoFreeTrialCode where
  showUser NoFreeTrialCode = "no free trial code found for this account"

data AdminSignupError
  = AdminSignupError UserSignupError
  | InvalidAdminCode
  | CompanyCreationFailed
  deriving (Eq,Show,Generic)
instance ToJSON AdminSignupError
instance FromJSON AdminSignupError
instance ShowUser AdminSignupError where
  showUser InvalidAdminCode = "invalid code, please ask lauren@aceinterviewprep.io for this code"
  showUser (AdminSignupError e) = "For Admin User: " <> showUser e
  showUser CompanyCreationFailed = "Failed to create company record"

data CancelSubError
  = NoSubOrAccount
  | SelfCancelUnderGroupPlan
  | NoStripeInfo
  | NoSubscriptionExists
  | StripeCouldntCancel StripeError
  deriving (Show,Generic)
instance ToJSON CancelSubError
instance FromJSON CancelSubError
instance ShowUser CancelSubError where
  showUser NoSubOrAccount = "No subscription found but also no account, this should not happen and error has been reported"
  showUser SelfCancelUnderGroupPlan = "Cancel subscription failed, you are registered under a bootcamp user plan"
  showUser NoStripeInfo = "Could not retrieve stripe info"
  showUser NoSubscriptionExists = "No subscription found. Your subscription is likely canceled"
  showUser (StripeCouldntCancel e) = errorMsg e


data OAuthError
  = InvalidOAuthEmail T.Text
  | AccountCreationFailed
  | NoAuthorizationCode
  | ProviderUserInfoFailed T.Text
  deriving (Eq,Show,Generic)
instance FromJSON OAuthError
instance ToJSON OAuthError
instance ShowUser OAuthError where
  showUser (InvalidOAuthEmail provider) = "Invalid email received from " <> provider
  showUser AccountCreationFailed = "Failed to create account"
  showUser NoAuthorizationCode = "No authorization code received from provider"
  showUser (ProviderUserInfoFailed provider) = "Failed to get user info from " <> provider

data LoginError
  = UnrecognizedEmail T.Text
  | IncorrectPassword
  | NoUserTypeFound
  | ExpiredSubscription T.Text
  | ExpiredFreeTrial T.Text
  deriving (Eq,Show,Generic)
instance FromJSON LoginError
instance ToJSON LoginError
instance ShowUser LoginError where
  showUser (UnrecognizedEmail _) = "An account with this email doesn't exist"
  showUser IncorrectPassword =  "Incorrect password, please try again"
  showUser NoUserTypeFound =  "No UserType found"
  showUser (ExpiredSubscription subscribeURL) =  "Your subscription is no longer active. Please visit " <> subscribeURL
  showUser (ExpiredFreeTrial userEmail) = "The free trial for " <> userEmail <> " is expired"




instance ToJSON ByteString where
    toJSON = toJSON . decodeUtf8 . B64.encode

instance FromJSON ByteString where
    parseJSON o = either fail return . B64.decode . encodeUtf8 =<< parseJSON o

instance ToJSON LBS.ByteString where
    toJSON = toJSON . decodeUtf8 . B64.encode . LBS.toStrict

instance FromJSON LBS.ByteString where
    parseJSON o = either fail (return . LBS.fromStrict) . B64.decode . encodeUtf8 =<< parseJSON o

instance ToJSONKey ByteString
instance FromJSONKey ByteString
