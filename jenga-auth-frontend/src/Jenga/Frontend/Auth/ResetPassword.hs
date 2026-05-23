{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Jenga.Frontend.Auth.ResetPassword where

-- import Common.Request
-- import Common.Route

import Jenga.Frontend.Api
import Jenga.Common.HasJengaConfig
import Jenga.Common.Auth
import Jenga.Common.Errors
-- import Templates.Partials.Buttons
-- import Templates.Partials.Errors
import Templates.Types

import Control.Monad.Fix
import Control.Monad.Reader
import Data.Functor.Identity
import Data.Signed (Signed)
import qualified Data.Text as T
import Obelisk.Configs
import Obelisk.Route.Frontend
import Rhyolite.Api (ApiRequest(..))
import Rhyolite.Account (PasswordResetToken)
import Data.Text (Text)
import Reflex.Dom.Core

data ResetPasswordConfig t = ResetPasswordConfig
  { _resetPasswordConfig_errors :: Dynamic t (Maybe Text)
  , _resetPasswordConfig_result :: Event t (AuthToken, UserType)
  }

data ResetPasswordData t m = ResetPasswordData
  { _resetPassword_password :: InputEl t m
  , _resetPassword_confirmPassword :: InputEl t m
  , _resetPassword_submit :: Event t ()
  }

-- resetPassword_TMPL :: Template t m => ResetPasswordConfig t -> m (ResetPasswordData t m)
-- resetPassword_TMPL cfg = do
--   authFormTemplate hectorRecommendation $ do
--     authFormTitle "Reset Password"
--     password <- authFormRow $ do
--       authFormLabel "Enter Password"
--       authFormTextInput "password" "Enter your password"
--     confirmPassword <- authFormRow $ do
--       authFormLabel "Confirm Password"
--       authFormTextInput "password" "One more time pleeeeeease"
--     -- elClass "h1" shell_1 $
--     --   textS reset_password "Reset Password"
--     -- password <- elClass "div" shell_2 $ mdo
--     --   elClass "div" "" $ textS "" "New Password"
--     --   inputElement $ def
--     --     & initialAttributes .~ ("type" =: "password")
--     -- confirmPassword <- elClass "div" shell_3 $ mdo
--     --   elClass "div" "" $ textS "" "Confirm Password"
--     --   inputElement $ def
--     --     & initialAttributes .~ ("type" =: "password")
--     maybeDisplay errorMessage $ _resetPasswordConfig_errors cfg
--     submit <- primaryButton "Confirm"
--     return $ ResetPassword
--       { _resetPassword_password = password
--       , _resetPassword_confirmPassword = confirmPassword
--       , _resetPassword_submit = submit
--       }
--   where
--     -- shell_1 = $(classh' [mt .~~ TWSize 12])
--     -- shell_2 = $(classh' [mt .~~ TWSize 4, custom .~ "flex flex-col"])
--     -- shell_3 = $(classh' [mt .~~ TWSize 8, custom .~ "flex flex-col"])

--     -- reset_password = $(classh' [ text_font .~~ Font_Custom "Karla"
--     --                            , text_weight .~~ Bold
--     --                            ])


resetPassword_FRP ::
  forall frontendRoute backendRoute token publicRequest privateRequest cfg t m.
  ( MonadFix m
  , MonadHold t m
  , DomBuilder t m
  , PostBuild t m
  , Routed t (Signed PasswordResetToken) m
  , Requester t m
  , HasConfigs (Client m)
  , Prerender t m
  , Request m ~ ApiRequest token publicRequest privateRequest
  , Response m ~ Identity
  , HasConfig cfg (FullRouteEncoder backendRoute frontendRoute)
  , HasConfig cfg BaseURL
  , MonadReader cfg m
  )
  => R backendRoute
  -> ( ResetPasswordData t m )
  -> m (ResetPasswordConfig t ) -- Event t (AuthToken, UserType))
resetPassword_FRP routeReset (ResetPasswordData pass conf submit) = do
  token <- askRoute
  rec
    -- ResetPassword pass conf submit <- resetPassword_TMPL $
    --   ResetPasswordConfig
    --     { _resetPasswordConfig_errors = errors
    --     }
    let credentials = tag (current $ (,,) <$> token <*> value pass <*> value conf) submit
        (credError, validCredentials) = fanEither $ ffor credentials $ \(tok, pw, cf) ->
          case (pw == cf, T.null pw) of
            (True, False) -> Right (tok, pw)
            (False, False) -> Left "Passwords don't match"
            (_, True) -> Left "Empty fields"
        -- request = ffor validCredentials $ \(tok, pw) -> ApiRequest_Public $ PublicRequest_ResetPassword tok pw
        -- (err, result) = fanEither response
    --response <- requestingIdentity request
    (err :: Event t (RequestError ResetPasswordError), result :: Event t (AuthToken, UserType)) <-
      runAPI @frontendRoute routeReset validCredentials
    errors <- holdDyn Nothing $ leftmost $ fmap (Just <$>) [credError, showUser <$> err]
  pure $ ResetPasswordConfig errors result
