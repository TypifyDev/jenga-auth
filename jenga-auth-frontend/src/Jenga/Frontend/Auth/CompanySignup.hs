{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Jenga.Frontend.Auth.CompanySignup where

--import Jenga.Frontend.MkTmpl
import Jenga.Common.Auth
import Templates.Partials.Checkbox
import Templates.Types
import Jenga.Common.Errors

import Rhyolite.Api (ApiRequest(..))
import Obelisk.Route.Frontend
import Reflex
import Reflex.Dom.Core hiding (checkbox, Checkbox(..), CheckboxConfig(..))

import Control.Monad (join, forM_)
import Control.Monad.Fix
import qualified Text.Email.Validate as EmailValidate
import qualified Data.Map as Map
import Data.Functor.Identity
import qualified Data.Text.Encoding as T
import qualified Data.Text as T


data SignupConfig t = SignupConfig
  { _signupConfig_errors :: Dynamic t (Maybe T.Text)
  , _signupConfig_success :: Event t ()
  }


data SignupData t m = SignupData
  { _signup_email :: InputEl t m
  , _signup_confirmEmail :: InputEl t m
  , _signup_orgName :: InputEl t m
  , _signup_agreeToTerms :: Checkbox t
  , _signup_submit :: Event t ()
  }


-- | TODO(anyone): It would be really cool to populate forms with info they've already entered so that they can move forward faste
-- | we could do this through query params
newCompanySignup_FRP
  :: ( DomBuilder t m
     , SetRoute t (R frontendRoute) m
     , RouteToUrl (R frontendRoute) m
     , Prerender t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Routed t (Map.Map T.Text (Maybe T.Text)) m
     , Requester t m
     , Request m ~ ApiRequest token publicRequest privateRequest
     , Response m ~ Identity
     )
  => (NewCompanyEmail -> ApiRequest token publicRequest privateRequest
      (Either (BackendError AdminSignupError) ())
     )
  --forall app. NewCompanyEmail -> PublicApi app (Either e a)
  -> (SignupData t m)
  -> m (SignupConfig t) --(Event t ()) --()
newCompanySignup_FRP mkAPI (SignupData email' eConfirm' orgName' agree' submit) = mdo
  --Signup email' eConfirm' orgName' agree' submit <- newCompanySignup_TMPL $ SignupConfig errors
  queryParams :: Dynamic t (Map.Map T.Text (Maybe T.Text)) <- askRoute
  let code = join . Map.lookup "code" <$> queryParams
  let email = value email'
  let eConfirm = value eConfirm'
  let agree = value agree'
  let orgName = value orgName'
  let
    validate = (\(emailA,emailB,orgName_, agree'', code') ->
           if agree'' == False
           then Left "You must accept terms"
           else
             if emailA /= emailB
             then Left "Emails dont match"
             else
               -- Code is no longer required at the frontend; absent → "".
               -- The backend handler stamps the correct value before
               -- 'matchesCompanyCodeEnv' runs, so this is safe.
               case EmailValidate.validate $ T.encodeUtf8 emailA of
                 Left _ -> Left "Invalid Email"
                 Right e ->
                   Right $ NewCompanyEmail e orgName_ (maybe "" id code')
               )
    valid = fmap validate $ (,,,,) <$> email <*> eConfirm <*> orgName <*> agree <*> code
  let (bad, good') = fanEither (tag (current valid) submit)
  let request = ffor good' $ mkAPI -- ApiRequest_Public . PublicRequest_CompanySignup
  response <- requestingIdentity request
  let (errRes, goodResponse) = fanEither response
  let errorsEv = Just <$> leftmost
        [ bad
        , showUser . Req . Request_ErrorAPI <$> errRes
        , "Success, please check your email" <$ goodResponse
        ]
  errors <- holdDyn Nothing errorsEv

  pure $ SignupConfig errors goodResponse -- (signupgoodResponse

seeTerms :: DomBuilder t m => m ()
seeTerms = do
  elClass "div" "bg-gray-100 flex items-center justify-center" $ do
    elClass "div" "bg-white p-8 rounded-3xl w-full overflow-y-auto h-[50vh]" $ do
    elClass "h1" "text-2xl font-[Sarabun] mb-4 text-gray-700" $ text "TERMS OF SERVICE AND PRIVACY POLICY"
    elClass "p" "text-base font-[Sarabun] mb-2 text-gray-700" $ text "Last Updated: March 23, 2023"
    forM_ paragraphsAgree $ elClass "p" "text-base font-[Sarabun] mb-2 text-gray-700" . text
  where
    paragraphsAgree =
      [ "This document (\"Agreement\") is an agreement between you (\"User\" or \"You\") and Ace, Inc., (\"Company,\" \"We,\" \"Us,\" or \"Our\"), governing your use of the Ace, Inc. platform and related services (collectively, the \"Services\")."
      , "By accessing or using the Services, you acknowledge that you have read, understood, and agree to be bound by this Agreement. If you do not agree to these terms, you are not permitted to use the Services."
      , "Eligibility"
      , "To use the Services, you must be at least 13 years of age. By using the Services, you represent and warrant that you meet these age requirements and can form legally binding contracts under applicable law. If you are using the Services on behalf of a company, organization, or other legal entity, you represent and warrant that you have the authority to bind that entity to this Agreement."
      , "Account Registration"
      , "To access certain features of the Services, you may be required to register for an account. By registering, you agree to provide accurate, current, and complete information about yourself and maintain the security of your account credentials. You are responsible for all activities that occur under your account, whether or not you authorize such activities."
      , "Content"
      , "User-Generated Content"
      , "The Services allow you to create, upload, post, send, or otherwise transmit content, including videos, messages, and other materials (\"User Content\"). By submitting User Content, you represent and warrant that you own or have the necessary rights to use and distribute such content."
      , "License Grant"
      , "By submitting User Content, you grant the Company a worldwide, non-exclusive, royalty-free, sublicensable, and transferable license to use, reproduce, distribute, prepare derivative works of, display, and perform the User Content in connection with the Services and the Company's (and its successors' and affiliates') business, including for promoting and redistributing part or all of the Services."
      , "Content Restrictions"
      , "You agree not to post, upload, or transmit any User Content that is unlawful, harmful, threatening, abusive, harassing, defamatory, vulgar, obscene, libelous, invasive of another's privacy, hateful, or racially, ethnically, or otherwise objectionable."
      , "Prohibited Conduct"
      , "You agree not to engage in any of the following activities while using the Services:"
      , "Using the Services for any purpose that is unlawful or prohibited by this Agreement;"
      , "Interfering with or disrupting the Services or servers or networks connected to the Services;"
      , "Attempting to gain unauthorized access to any portion of the Services or any other user's account;"
      , "Using the Services to harass, abuse, or harm other individuals;"
      , "Creating multiple accounts or using automated systems to access the Services."
      , "Intellectual Property Rights"
      , "The Services, including all content, features, and functionality (including but not limited to all information, software, text, displays, images, video, and audio, and the design, selection, and arrangement thereof), are owned by the Company, its licensors, or other providers of such material and are protected by copyright, trademark, patent, trade secret, and other intellectual property or proprietary rights laws."
      , "Privacy Policy"
      , "Please refer to our Privacy Policy, which is incorporated into this Agreement by reference, for information on how we collect, use, and share your personal information."
      , "Termination"
      , "The Company may terminate your access to the Services at any time, with or without cause, and without prior notice. You may terminate your account by following the instructions on the Services. Upon termination, you shall cease all use of the Services and any content or materials obtained through the Services. Any provisions of this Agreement that, by their nature, should survive termination shall survive termination, including, without limitation, ownership provisions, warranty disclaimers, indemnity, and limitations of liability."
      , "Disclaimers"
      , "The Services are provided on an 'as is' and 'as available' basis, without warranty of any kind, express or implied, including, but not limited to, warranties of merchantability, fitness for a particular purpose, title, and non-infringement. The Company does not warrant that the Services will be error-free, uninterrupted, secure, or free from viruses or other harmful components."
      , "Limitation of Liability"
      , "In no event shall the Company, its affiliates, or their respective officers, directors, employees, agents, licensors, or service providers be liable for any direct, indirect, incidental, special, consequential, or punitive damages, including, without limitation, loss of profits, data, use, goodwill, or other intangible losses, resulting from your access to or use of, or inability to access or use, the Services, any conduct or content of any third party on the Services, or unauthorized access, use, or alteration of your transmissions or content, whether based on warranty, contract, tort (including negligence), or any other legal theory."
      , "Indemnification"
      , "You agree to defend, indemnify, and hold harmless the Company, its affiliates, licensors, and service providers, and its and their respective officers, directors, employees, contractors, agents, licensors, suppliers, successors, and assigns from and against any claims, liabilities, damages, judgments, awards, losses, costs, expenses, or fees (including reasonable attorneys' fees) arising out of or relating to your violation of this Agreement or your use of the Services, including, but not limited to, your User Content, any use of the Services' content, services, and products other than as expressly authorized in this Agreement, or your use of any information obtained from the Services."
      , "Governing Law and Jurisdiction"
      , "This Agreement shall be governed by and construed in accordance with the laws of Canada, without giving effect to any principles of conflicts of law. You agree that any action at law or in equity arising out of or relating to this Agreement shall be filed only in the state or federal courts located in Canada and you hereby consent and submit to the personal jurisdiction of such courts for the purposes of litigating any such action."
      , "Changes to this Agreement"
      , "We reserve the right to modify this Agreement at any time, and any such changes will be effective immediately upon posting. By continuing to use the Services after any changes, you agree to be bound by the revised Agreement. If you do not agree to the new terms, you must stop using the Services."
      , "Miscellaneous"
      , "This Agreement constitutes the entire agreement between you and the Company concerning your use of the Services and supersedes all prior or contemporaneous communications, proposals, and agreements between you and the Company with respect to the subject matter hereof. If any provision of this Agreement is found to be unlawful, void, or for any reason unenforceable, then that provision shall be deemed severable from this Agreement and shall not affect the validity and enforceability of any remaining provisions. No waiver by the Company of any term or condition set forth in this Agreement shall be deemed a further or continuing waiver of such term or condition or a waiver of any other term or condition, and any failure of the Company to assert a right or provision under this Agreement shall not constitute a waiver of such right or provision."
      ]
