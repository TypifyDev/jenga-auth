module Jenga.Backend.Handlers.Auth.OAuth where

import Jenga.Backend.DB.Auth
-- import Backend.DB

-- import Backend.Utils.Log
import Jenga.Backend.Utils.Snap
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Cookies (addAuthCookieHeader, addUserTypeCookieHeader)
import Jenga.Common.Schema
import Jenga.Common.OAuth
import Jenga.Common.Auth
import Jenga.Common.BeamExtras (Id)

import Rhyolite.Account
import Obelisk.Route
import Obelisk.OAuth.AccessToken (TokenRequest (..), TokenGrant (..), getOauthToken)
import Obelisk.OAuth.Authorization (OAuth (..), RedirectUriParams (..))
import Database.Beam.Postgres
import Database.Beam.Schema
import Database.Beam.Backend.SQL.Types (SqlSerial(..))

import Snap
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Header as Http
import Network.HTTP.Client.TLS
import Web.ClientSession as CS
import Data.Pool
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Dependent.Sum
import Data.Maybe (isJust, fromMaybe)
import Data.List (find)
import Data.Functor.Identity
import Data.Int (Int64)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Email.Validate as EmailValidate

-- | Helper function to handle OAuth login with email-based account lookup
-- Validates email, creates or links account, and sets cookies
handleOAuthLogin
  :: forall db beR cfg frontendRoute oauthIdRow m.
     ( MonadSnap m
     , MonadIO m
     , Database Postgres db
     , HasConfig cfg AuthCookieName
     , HasConfig cfg UserTypeCookieName
     , HasConfig cfg DomainOption
     , HasConfig cfg CS.Key
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg BaseURL
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     )
  => String  -- ^ Email from OAuth provider
  -> Maybe oauthIdRow  -- ^ Existing OAuth ID row if user has logged in before
  -> (oauthIdRow -> Int64)  -- ^ Extract account ID from OAuth ID row
  -> (Id Account -> ReaderT cfg m ())  -- ^ Insert new OAuth ID for account
  -> R frontendRoute  -- ^ Where to redirect after success
  -> ReaderT cfg m (Id Account)
handleOAuthLogin emailStr maybeOAuthID extractAccountId insertOAuthId redirectToRoute = do
  -- Validate email format
  case EmailValidate.validate (T.encodeUtf8 $ T.pack emailStr) of
    Left _err -> error $ "Invalid email format from OAuth provider: " <> emailStr
    Right _validEmail -> do
      accountID' <- case maybeOAuthID of
        Nothing -> do
          -- New user signup or link to existing account
          (acctsTbl :: PgTable Postgres db Account) <- asksTableM

          -- Check if account with this email already exists
          maybeExistingAcct <- withDbEnv $ getUserByEmail acctsTbl (T.pack emailStr)
          liftIO $ print $ isJust maybeExistingAcct
          case maybeExistingAcct of
            -- Link to existing account
            Just existingAcct -> do
              let existingAccountId = pk existingAcct
              insertOAuthId existingAccountId
              pure existingAccountId
            -- Create new account
            Nothing -> do
              accountID <- withDbEnv $ newAccount acctsTbl (Email $ T.pack emailStr) Nothing
              case accountID of
                Nothing -> error "Insert new account failed"
                Just aid -> do
                  insertOAuthId aid
                  -- Create user type for new OAuth user
                  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
                  withDbEnv $ putNewUserType uTypeTbl aid Nothing
                  pure aid

        Just oauthIdRow -> do
          -- Existing user login
          pure $ AccountId $ SqlSerial $ extractAccountId oauthIdRow

      setCookiesAndRedirect @db @beR accountID' redirectToRoute
      pure accountID'

-- | Helper function to set both auth and user type cookies after OAuth login
setCookiesAndRedirect
  :: forall db beR cfg frontendRoute m.
     ( MonadSnap m
     , MonadIO m
     , Database Postgres db
     , HasConfig cfg AuthCookieName
     , HasConfig cfg UserTypeCookieName
     , HasConfig cfg DomainOption
     , HasConfig cfg CS.Key
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg BaseURL
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasJengaTable Postgres db UserTypeTable
     )
  => Id Account
  -> R frontendRoute
  -> ReaderT cfg m ()
setCookiesAndRedirect accountID' redirectToRoute = do
  liftIO $ putStrLn $ "setCookiesAndRedirect called for account: " <> show accountID'
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  userType <- withDbEnv $ getUserType uTypeTbl accountID'

  -- If user doesn't have a type, create one
  userType' <- case userType of
    Just uType -> pure uType
    Nothing -> do
      liftIO $ putStrLn $ "Creating missing user type for existing OAuth user: " <> show accountID'
      withDbEnv $ putNewUserType uTypeTbl accountID' Nothing
      pure Self

  liftIO $ putStrLn $ "About to set cookies for user type: " <> show userType'
  userTypeCookieName <- getUserTypeCookieName <$> asksM
  liftIO $ putStrLn $ "User type cookie name: " <> show userTypeCookieName
  addUserTypeCookieHeader userTypeCookieName userType'
  liftIO $ putStrLn "User type cookie set, now setting auth cookie"
  authCookieName <- getAuthCookieName <$> asksM
  liftIO $ putStrLn $ "Auth cookie name: " <> show authCookieName
  addAuthCookieHeader authCookieName accountID'
  liftIO $ putStrLn "Both cookies set, checking response headers before redirect"

  -- Debug: inspect response headers before redirect
  resp <- Snap.getResponse
  let headers_ = Snap.listHeaders resp
  liftIO $ putStrLn $ "Response headers before redirect: " <> show headers_

  frontendRedirect @beR redirectToRoute
  pure ()

-- TODO: they dont need to reset password if they have signed up with github
-- | TODO: not actually in use, should actually work no problem with github
oauthHandler
  :: forall db beR cfg frontendRoute m.
     ( MonadSnap m
     , Database Postgres db
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg BaseURL
     , HasConfig cfg CS.Key
     , HasConfig cfg AuthCookieName
     , HasConfig cfg DomainOption
     , HasConfig cfg UserTypeCookieName
     , HasConfig cfg GithubOAuthClientSecret
     , HasConfig cfg GithubOAuthClientID
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db GithubID
     , HasJengaTable Postgres db UserTypeTable
     )
  => DSum OAuth Identity
  -> beR (R OAuth)
  -> R frontendRoute
  -> R frontendRoute
  -> ReaderT cfg m (Maybe (Id Account, T.Text))
oauthHandler oauthRoute oAuthRedirectGADT redirectToRoute redirectNoAuth = case oauthRoute of
  OAuth_RedirectUri :/ redirectParams -> case redirectParams of
    Nothing -> liftIO $ error "Expected to receive the authorization code here"
    Just (RedirectUriParams code _mstate) -> do
      clientId <- getGithubOAuthClientID <$> asksM
      clientSecret <- getGithubOAuthClientSecret <$> asksM
      route' <- T.pack . show . getBaseURL <$> asksM
      let t = TokenRequest
            { _tokenRequest_grant = TokenGrant_AuthorizationCode $ T.encodeUtf8 code
            , _tokenRequest_clientId = clientId
            , _tokenRequest_clientSecret = clientSecret
            , _tokenRequest_redirectUri = (\x -> oAuthRedirectGADT :/ x)
            }
          oAuthUrl = "https://github.com/login/oauth/access_token"
      tlsMgr <- liftIO $ Http.newManager tlsManagerSettings

      (checkedEncoder :: FullRouteEncoder beR frontendRoute) <- asksM

      req <- liftIO $ getOauthToken oAuthUrl route' checkedEncoder t
      rsp <- liftIO $ flip Http.httpLbs tlsMgr (req { Http.requestHeaders = Http.requestHeaders req
                                                      <> [(Http.hAccept, "application/json")] }
                                               )
      let accessToken = fmap access_token . Aeson.decode . Http.responseBody $ rsp
      case accessToken of
        Nothing -> do
          frontendRedirect @beR redirectNoAuth
          pure Nothing
        Just aToken -> do
          reqUser <- liftIO $ Http.parseRequest "https://api.github.com/user"
          let reqUser' = reqUser { Http.requestHeaders = Http.requestHeaders reqUser <>
                                   [ (Http.hAuthorization, "Bearer " <> (T.encodeUtf8 aToken))
                                   , (Http.hUserAgent, "Ace Interview Prep Haskell Server")
                                   ]
                                 }
          res <- liftIO $ flip Http.httpLbs tlsMgr reqUser'
          let userGithub :: Maybe GitHubUser = Aeson.decode . Http.responseBody $ res
          case userGithub of
            Nothing -> error "this is likely a bug or you do not have a github account"
            Just userGH -> do
              ghEmail <- case _githubUser_github_email userGH of
                Just email -> pure email
                Nothing -> do
                  reqEmails <- liftIO $ Http.parseRequest "https://api.github.com/user/emails"
                  let reqEmails' = reqEmails { Http.requestHeaders = Http.requestHeaders reqEmails <>
                                              [ (Http.hAuthorization, "Bearer " <> (T.encodeUtf8 aToken))
                                              , (Http.hUserAgent, "Ace Interview Prep Haskell Server")
                                              ]
                                            }
                  resEmails <- liftIO $ flip Http.httpLbs tlsMgr reqEmails'
                  let emails = Aeson.decode . Http.responseBody $ resEmails :: Maybe [GitHubEmail]
                  case emails of
                    Just emailList -> do
                      let primaryEmail = find (\e -> _githubEmail_primary e && _githubEmail_verified e) emailList
                      case primaryEmail of
                        Just e -> pure $ _githubEmail_email e
                        Nothing -> pure $ _githubUser_login userGH
                    Nothing -> pure $ _githubUser_login userGH

              (ghTbl :: PgTable Postgres db GithubID) <- asksTableM
              maybeGitID <- withDbEnv $ getGithubUserIfTheyExist ghTbl userGH

              liftIO $ putStrLn ghEmail
              let ghDisplayName = T.pack $ fromMaybe (_githubUser_login userGH) (_githubUser_name userGH)
              acctId <- handleOAuthLogin @db @beR
                ghEmail
                maybeGitID
                (\(GithubID _ghid uid) -> uid)
                (\aid -> withDbEnv $ insertNewGithubID ghTbl userGH aid)
                redirectToRoute
              pure $ Just (acctId, ghDisplayName)

-- | Google OAuth handler
googleOAuthHandler
  :: forall db beR cfg frontendRoute m.
     ( MonadSnap m
     , Database Postgres db
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg BaseURL
     , HasConfig cfg CS.Key
     , HasConfig cfg AuthCookieName
     , HasConfig cfg DomainOption
     , HasConfig cfg UserTypeCookieName
     , HasConfig cfg GoogleOAuthClientSecret
     , HasConfig cfg GoogleOAuthClientID
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db GoogleID
     , HasJengaTable Postgres db UserTypeTable
     )
  => DSum OAuth Identity
  -> beR (R OAuth)
  -> R frontendRoute
  -> R frontendRoute
  -> ReaderT cfg m (Maybe (Id Account, T.Text))
googleOAuthHandler oauthRoute oAuthRedirectGADT redirectToRoute redirectNoAuth = case oauthRoute of
  OAuth_RedirectUri :/ redirectParams -> case redirectParams of
    Nothing -> liftIO $ error "Expected to receive the authorization code here"
    Just (RedirectUriParams code _mstate) -> do
      clientId <- getGoogleOAuthClientID <$> asksM
      clientSecret <- getGoogleOAuthClientSecret <$> asksM
      route' <- T.pack . show . getBaseURL <$> asksM
      let t = TokenRequest
            { _tokenRequest_grant = TokenGrant_AuthorizationCode $ T.encodeUtf8 code
            , _tokenRequest_clientId = clientId
            , _tokenRequest_clientSecret = clientSecret
            , _tokenRequest_redirectUri = (\x -> oAuthRedirectGADT :/ x)
            }
          oAuthUrl = "https://oauth2.googleapis.com/token"
      tlsMgr <- liftIO $ Http.newManager tlsManagerSettings

      (checkedEncoder :: FullRouteEncoder beR frontendRoute) <- asksM

      req <- liftIO $ getOauthToken oAuthUrl route' checkedEncoder t
      rsp <- liftIO $ flip Http.httpLbs tlsMgr (req { Http.requestHeaders = Http.requestHeaders req
                                                      <> [(Http.hAccept, "application/json")] }
                                               )
      let accessToken = fmap access_token . Aeson.decode . Http.responseBody $ rsp
      case accessToken of
        Nothing -> do
          frontendRedirect @beR redirectNoAuth
          pure Nothing
        Just aToken -> do
          reqUser <- liftIO $ Http.parseRequest "https://www.googleapis.com/oauth2/v2/userinfo"
          let reqUser' = reqUser { Http.requestHeaders = Http.requestHeaders reqUser <>
                                   [ (Http.hAuthorization, "Bearer " <> (T.encodeUtf8 aToken))
                                   ]
                                 }
          res <- liftIO $ flip Http.httpLbs tlsMgr reqUser'
          let userGoogle :: Maybe GoogleUser = Aeson.decode . Http.responseBody $ res
          case userGoogle of
            Nothing -> error "Failed to get Google user info"
            Just userG -> do
              (googleTbl :: PgTable Postgres db GoogleID) <- asksTableM
              maybeGoogleID <- withDbEnv $ getGoogleUserIfTheyExist googleTbl userG

              let googleEmail = _googleUser_email userG
                  googleDisplayName = T.pack $ fromMaybe (takeWhile (/= '@') googleEmail) (_googleUser_name userG)
              acctId <- handleOAuthLogin @db @beR
                googleEmail
                maybeGoogleID
                (\(GoogleID _gid uid) -> uid)
                (\aid -> withDbEnv $ insertNewGoogleID googleTbl userG aid)
                redirectToRoute
              pure $ Just (acctId, googleDisplayName)

-- | Discord OAuth handler
discordOAuthHandler
  :: forall db beR cfg frontendRoute m.
     ( MonadSnap m
     , Database Postgres db
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg BaseURL
     , HasConfig cfg CS.Key
     , HasConfig cfg AuthCookieName
     , HasConfig cfg DomainOption
     , HasConfig cfg UserTypeCookieName
     , HasConfig cfg DiscordOAuthClientSecret
     , HasConfig cfg DiscordOAuthClientID
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db DiscordID
     , HasJengaTable Postgres db UserTypeTable
     )
  => DSum OAuth Identity
  -> beR (R OAuth)
  -> R frontendRoute
  -> R frontendRoute
  -> ReaderT cfg m (Maybe (Id Account, T.Text))
discordOAuthHandler oauthRoute oAuthRedirectGADT redirectToRoute redirectNoAuth = case oauthRoute of
  OAuth_RedirectUri :/ redirectParams -> case redirectParams of
    Nothing -> liftIO $ error "Expected to receive the authorization code here"
    Just (RedirectUriParams code _mstate) -> do
      clientId <- getDiscordOAuthClientID <$> asksM
      clientSecret <- getDiscordOAuthClientSecret <$> asksM
      route' <- T.pack . show . getBaseURL <$> asksM
      let t = TokenRequest
            { _tokenRequest_grant = TokenGrant_AuthorizationCode $ T.encodeUtf8 code
            , _tokenRequest_clientId = clientId
            , _tokenRequest_clientSecret = clientSecret
            , _tokenRequest_redirectUri = (\x -> oAuthRedirectGADT :/ x)
            }
          oAuthUrl = "https://discord.com/api/oauth2/token"
      tlsMgr <- liftIO $ Http.newManager tlsManagerSettings

      (checkedEncoder :: FullRouteEncoder beR frontendRoute) <- asksM

      req <- liftIO $ getOauthToken oAuthUrl route' checkedEncoder t
      rsp <- liftIO $ flip Http.httpLbs tlsMgr (req { Http.requestHeaders = Http.requestHeaders req
                                                      <> [(Http.hAccept, "application/json")] }
                                               )
      let accessToken = fmap access_token . Aeson.decode . Http.responseBody $ rsp
      case accessToken of
        Nothing -> do
          frontendRedirect @beR redirectNoAuth
          pure Nothing
        Just aToken -> do
          reqUser <- liftIO $ Http.parseRequest "https://discord.com/api/users/@me"
          let reqUser' = reqUser { Http.requestHeaders = Http.requestHeaders reqUser <>
                                   [ (Http.hAuthorization, "Bearer " <> (T.encodeUtf8 aToken))
                                   ]
                                 }
          res <- liftIO $ flip Http.httpLbs tlsMgr reqUser'
          let userDiscord :: Maybe DiscordUser = Aeson.decode . Http.responseBody $ res
          case userDiscord of
            Nothing -> error "Failed to get Discord user info"
            Just userD -> do
              (discordTbl :: PgTable Postgres db DiscordID) <- asksTableM
              maybeDiscordID <- withDbEnv $ getDiscordUserIfTheyExist discordTbl userD

              let discordEmail = fromMaybe (_discordUser_username userD) $ _discordUser_email userD
                  discordDisplayName = T.pack $ _discordUser_username userD
              acctId <- handleOAuthLogin @db @beR
                discordEmail
                maybeDiscordID
                (\(DiscordID _did uid) -> uid)
                (\aid -> withDbEnv $ insertNewDiscordID discordTbl userD aid)
                redirectToRoute
              pure $ Just (acctId, discordDisplayName)
