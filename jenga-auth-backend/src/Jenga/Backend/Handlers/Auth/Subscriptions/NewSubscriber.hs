module Jenga.Backend.Handlers.Auth.Subscriptions.NewSubscriber where

import Jenga.Backend.DB.Subscriptions
import Jenga.Backend.DB.OrgBased
import Jenga.Backend.DB.Auth
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Email
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Common.BeamExtras
import Jenga.Common.Stripe
import Jenga.Common.Schema

import Rhyolite.Account as Rhy
import Rhyolite.Backend.Account as RhyB
import Database.Beam.Schema
import Database.Beam.Postgres
import Obelisk.Route
import qualified Reflex.Dom.Core as Rfx
import Data.Signed

import Network.Mail.Mime
import Web.Stripe as S
import Web.Stripe.Customer as S
import Web.Stripe.Subscription as S
import Web.Stripe.Token as S
import Web.Stripe.Plan as S
import Web.ClientSession as CS

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Pool
import Data.Aeson (FromJSON)
import Data.Time.Clock (nominalDay, addUTCTime, getCurrentTime)
import qualified Data.Text as T

newtype StripeCode = StripeCode T.Text

-- Change to Confirm Subscription
newSubscriberHandler
  :: forall db beR cfg be frontendRoute m n.
     ( MonadIO m
     , Database Postgres db
     , HasConfig cfg StripeConfig
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg Plans
     , HasConfig cfg CS.Key
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasConfig cfg BaseURL
     , HasConfig cfg AdminEmail
     , HasJengaTable Postgres db Rhy.Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db FreeTrial
     , HasJengaTable Postgres db StripeRelation
     , HasJengaTable Postgres db OrgOwnedUsers
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => Id Rhy.Account
  -> PaymentFormPrivate
  -> frontendRoute (Signed PasswordResetToken)
  -> ReaderT cfg m (Either (BackendError SubscribeError) Bool)
newSubscriberHandler acctID form resetRoute = do
  stripeConfig <- asksM --  _stripeConfig
  (acctTbl :: PgTable Postgres db Rhy.Account) <- asksTableM
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  (freeTrialTbl :: PgTable Postgres db FreeTrial) <- asksTableM
  (stripeTbl :: PgTable Postgres db StripeRelation) <- asksTableM
  (orgTbl :: PgTable Postgres db OrgOwnedUsers) <- asksTableM

  email'_code <- withDbEnv $ do
    e <- getUsersEmail acctTbl acctID
    dc <- getUsersDiscountCode freeTrialTbl acctID
    pure $ (,) <$> e <*> dc
  case email'_code of
    Nothing -> pure $ Left . BCritical $ MissingEmail --"email not found"
    Just (email, discountCode) -> do
      let
        cardNum = _paymentForm_cardNumber form
        expMonth = _paymentForm_expiryMonth form
        expYear = _paymentForm_expiryYear form
        cvc = _paymentForm_cvc form
        userCard = (mkNewCard (CardNumber cardNum) (ExpMonth expMonth) (ExpYear expYear)) { newCardCVC = Just (CVC cvc) }
        stripe' :: (MonadIO m2, FromJSON (StripeReturn a)) => StripeRequest a -> m2 (Either StripeError (StripeReturn a))
        stripe' = liftIO . stripe stripeConfig
      liftIO $ print discountCode
      liftIO $ print userCard
      eithCardToken <- liftIO $ stripe' $ createCardToken (Just userCard)
      case eithCardToken of
        Left e -> pure . Left . BCritical . StripeGenError $ e
        Right token -> do
          liftIO $ print token
          eithCustomer :: Either StripeError Customer <- stripe' $ createCustomer
                                                         -&- (S.Email email)
                                                         -&- (tokenId token)
          case eithCustomer of
            Left stripeError -> pure . Left . BCritical . StripeGenError $ stripeError
            Right customer -> do
              liftIO $ print customer
              --StripeCode validCode <- lookupSubscriptionCodeEnv discountCode
              mValidCode <- lookupSubscriptionCodeEnv discountCode
              case mValidCode of
                Nothing -> pure . Left . BUserError $ NoMatchingPlanFound
                Just (StripePlan validCode)  -> do
                  planResult <- stripe' . getPlan $ PlanId validCode
                  case planResult of
                    Left stripeError2 -> pure . Left . BCritical . StripeGenError $ stripeError2
                    Right plan -> do
                      liftIO $ print plan
                      sevenDaysFromNow <- liftIO $ ((nominalDay * 7) `addUTCTime`) <$> getCurrentTime
                      let trialEnd = TrialEnd sevenDaysFromNow
                      eAccount <- withDbEnv $ ensureAccountExists' acctTbl email
                      case eAccount of
                        Left (EnsureAccount_InsertReturnedUnexpectedRows _) ->
                          pure $ Left . BCritical $ MissingEmail
                        Right (False, aid) -> do
                          mInfo <- withDbEnv $ getStripeInfo stripeTbl aid
                          case mInfo of
                            Nothing -> do
                              withDbEnv $ isOrgOwnedUser orgTbl aid >>= \case
                                False -> pure $ Left . BCritical $ NoAssociatedStripeInfo
                                True -> pure $ Left . BUserError $ AlreadyInGroupSubscription
                            Just (StripeRelation _ _ subId) ->
                              case subId of
                                Just _ -> pure $ Left . BUserError $ AlreadyInSelfSubscription
                                Nothing -> do
                                  -- Remove trial end, we handle this ourselves
                                  eithSubscription <- stripe' $ createSubscription (customerId customer) (planId plan) -&- trialEnd
                                  case eithSubscription of
                                    Left subscriptionError -> do
                                      pure . Left . BCritical . StripeGenError $ subscriptionError
                                    Right sub -> do
                                      withDbEnv $ do
                                        recordResubscribe stripeTbl aid (customerId customer) (subscriptionId sub)
                                      pure $ Right False
                        Right (True, aid) -> do
                          mNonce <- withDbEnv $ do
                            putNewUserType uTypeTbl aid Nothing
                            newNonce acctTbl aid
                          case mNonce of
                            Nothing -> pure $ Left . BCritical $ Stripe_FailedMkNonce
                            Just noncense -> do
                              csk <- asksM -- Cfg _clientSessionKey
                              authToken <- withDbEnv $ passwordResetToken csk aid noncense
                              eithSubscription <- stripe' $ createSubscription (customerId customer) (planId plan) -&- trialEnd
                              case eithSubscription of
                                Left subscriptionError -> do
                                  withDbEnv $ deleteFailedAccount acctTbl email
                                  pure . Left . BCritical . StripeGenError $ subscriptionError
                                Right sub -> do
                                  withDbEnv $ putNewStripeInfo stripeTbl aid (customerId customer) (subscriptionId sub)
                                  link <- renderFullRouteFE @beR $ resetRoute :/ authToken
                                  let
                                    to = Address
                                      { addressName = Nothing
                                      , addressEmail = email
                                      }
                                  sendEmail <- newEmailHtml @db [to] "Subscription Confirmation" $ do
                                    Rfx.el "div" $ do
                                      Rfx.el "div" $ Rfx.text "New account creation requested"
                                      Rfx.el "div" $ Rfx.text "go the following address to set your password:"
                                      Rfx.el "div" $ Rfx.text $ getLink link
                                      Rfx.el "div" $ Rfx.text ""
                                      Rfx.el "div" $ Rfx.text "if you didn't request this action, please ignore this email."
                                  case sendEmail of
                                    Left _ -> pure . Left . BCritical $ SubFailedEmail email
                                    Right _ -> pure $ Right True
