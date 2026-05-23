{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Jenga.Frontend.Auth.Login where

import Jenga.Frontend.Api (runAPIWithHeaders)
-- import Common.Request
-- import Common.Route

import Jenga.Frontend.Platform
import Jenga.Common.HasJengaConfig
import Templates.Types
import Jenga.Common.Auth
import Jenga.Common.Errors

import Obelisk.Configs
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Control.Monad.Fix
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.Reader

data LoginConfig t = LoginConfig
  { _loginConfig_errors :: Dynamic t (Maybe T.Text)
  , _loginConfig_submitEnabled :: Dynamic t Bool
  , _loginConfig_token :: Event t (AuthToken, UserType)
  }

data LoginData t m = LoginData
  { _login_username :: InputEl t m
  , _login_password :: InputEl t m
  , _login_submit :: Event t ()
  , _login_forgotPassword :: Event t ()
  , _login_goSignup :: Event t ()
  }

login_FRP ::
  forall frontendRoute backendRoute cfg t m.
  ( MonadFix m
  , MonadHold t m
  , DomBuilder t m
  , PostBuild t m
  , SetRoute t (R frontendRoute) m
  , HasConfigs (Client m)
  , Prerender t m
  , Requester t m
  -- , Request m ~ ApiRequest token publicRequest privateRequest
  --, Response m ~ Identity
  , HasConfig cfg (FullRouteEncoder backendRoute frontendRoute)
  , HasConfig cfg BaseURL
  , MonadReader cfg m
  )
  => R backendRoute
  -> R frontendRoute
  -> R frontendRoute
  -> LoginData t m
  -> m (LoginConfig t) -- (Event t (AuthToken, UserType))
login_FRP loginRoute requestNewPasswordPageRoute signupRoute (LoginData user pass submit forgotPass goSignup) = do
  rec
    let credentials = tag (current $ (,) <$> value user <*> value pass) submit
    (err :: Event t (RequestError LoginError), token :: Event t (AuthToken, UserType)) <- do
      runAPIWithHeaders @frontendRoute loginRoute (M.fromList [(clientTypeHeader, T.pack . show $ clientType)]) credentials
    shouldSubmit <- holdDyn True $ leftmost [False <$ credentials, True <$ token, True <$ err]
    errors <- holdDyn Nothing $ Just . showUser <$> err
    setRoute $ requestNewPasswordPageRoute <$ forgotPass
    setRoute $ signupRoute <$ goSignup
  pure LoginConfig
    { _loginConfig_errors = errors
    , _loginConfig_submitEnabled = shouldSubmit
    , _loginConfig_token = token
    }
  --pure token

-- loginTemplate'
--   :: Template t m
--   => LoginConfig t
--   -> m (Login t m)
-- loginTemplate' cfg = authFormTemplate hectorRecommendation $ do
--   authFormTitle "Log In"
--   username <- authFormRow $ do
--     authFormLabel "Email"
--     authFormTextInput "email" "Enter your email"
--   password <- authFormRow $ do
--     authFormLabel "Password"
--     authFormTextInput "password" "Enter your password"
--   maybeDisplay errorMessage $ _loginConfig_errors cfg
--   submit <- primaryButton' (_loginConfig_submitEnabled cfg) "Log in"
--   forgotPassword <- fmap (domEvent Click . fst) $ do
--     elClass' "div" $(classh' [w .~~ TWSize_Full
--                              , custom .~ "text-center"
--                              , mt .~~ TWSize 8
--                              ]) $
--       textS $(classh' [ text_font .~~ Font_Custom "Sarabun"
--                       , text_color .|~ [White, White, Gray C500]
--                       , text_size .~~ Base
--                       , custom .~ "cursor-pointer"
--                       ]) "Forgot password?"
--   return $ Login username password submit forgotPassword
