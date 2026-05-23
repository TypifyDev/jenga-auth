{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Api where

import Jenga.Common.HasJengaConfig
import Jenga.Common.Errors
import Obelisk.Configs
import Obelisk.Route
import Reflex.Dom.Core

import Control.Monad.Reader
import Data.Typeable
import Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Key as Aeson
import Text.Parsec
import Language.Javascript.JSaddle
import Control.Applicative
import Data.Bifunctor
import Data.Maybe
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS

eitherDecodeText :: FromJSON a => T.Text -> Either String a
eitherDecodeText = eitherDecode . LBS.fromStrict . T.encodeUtf8

-- | Convenience function to decode JSON-encoded responses.
-- | Note that the function this is adapted from (decodeXhrResponse also uses _xhrResponse_responseText
decodeXhrResponse' :: FromJSON a => XhrResponse -> Either String a
decodeXhrResponse' = (fromMaybe $ Left "no response text") . fmap eitherDecodeText  . _xhrResponse_responseText

type RunAPI t m =
  ( HasConfigs (Client m)
  , Prerender t m
  , Applicative m
  , DomBuilder t m
  , MonadHold t m
  , MonadFix m
  , PostBuild t m
  )



-- | Generic Req -> Response function
runAPI
  :: forall fe backendRoute cfg toJson fromJson t m err.
     ( ToJSON toJson
     , FromJSON fromJson
     , Typeable fromJson
     , Typeable err
     , FromJSON err
     , HasConfigs (Client m)
     , Prerender t m
     , Applicative m
     , DomBuilder t m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     , MonadReader cfg m
     )
  => R backendRoute
  -> Event t toJson
  -> m (Event t (RequestError err), Event t fromJson)
runAPI route evPayload = runAPIWithHeaders @fe route mempty evPayload

-- | Generic Req -> Response function
runAPIWithHeaders
  :: forall fe backendRoute cfg toJson fromJson t m err.
     ( ToJSON toJson
     , FromJSON fromJson
     , Typeable fromJson
     , Typeable err
     , FromJSON err
     , HasConfigs (Client m)
     , Prerender t m
     , Applicative m
     , DomBuilder t m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     , MonadReader cfg m
     )
  => R backendRoute
  -> Map.Map T.Text T.Text
  -> Event t toJson
  -> m (Event t (RequestError err), Event t fromJson)
runAPIWithHeaders route headers evPayload = do
  cfg <- ask
  fmap fanResponse' $ runRequest $
    flip runReaderT cfg $ performJSONRequestResponseAnnotatedWithHeaders @fe route headers evPayload

-- | Mutually exclusive to when you would use runAPIResponseGated
runAPIPostBuild
  :: forall fe backendRoute cfg toJson fromJson t m err.
     ( ToJSON toJson
     , FromJSON err
     , FromJSON fromJson
     , Typeable fromJson
     , Typeable err
     , HasConfigs (Client m)
     , Prerender t m
     , Applicative m
     , DomBuilder t m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     , MonadReader cfg m
     )
  => R backendRoute
  -> (Event t () -> Event t toJson)
  -> m (Event t (RequestError err), Event t fromJson)
runAPIPostBuild route withPb = runAPIPostBuildWithHeaders @fe route mempty withPb
  -- do
  -- fmap fanResponse' $ runRequest' $ performWithPb
  -- where
  --   runRequest' req = fmap switchDyn $ prerender (pure never) $ getPostBuild >>= req
  --   performWithPb = performJSONRequestResponseAnnotated route . withPb

-- | Mutually exclusive to when you would use runAPIResponseGated
runAPIPostBuildWithHeaders
  :: forall fe backendRoute cfg toJson fromJson t m err.
     ( ToJSON toJson
     , FromJSON err
     , FromJSON fromJson
     , Typeable fromJson
     , Typeable err
     , HasConfigs (Client m)
     , Prerender t m
     , Applicative m
     , DomBuilder t m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     , MonadReader cfg m
     )
  => R backendRoute
  -> Map.Map T.Text T.Text
  -> (Event t () -> Event t toJson)
  -> m (Event t (RequestError err), Event t fromJson)
runAPIPostBuildWithHeaders route headers withPb = do
  cfg <- ask
  fmap fanResponse' $ runRequest' $ \pb -> flip runReaderT cfg (performWithPb pb)
  where
    runRequest' req = fmap switchDyn $ prerender (pure never) $ getPostBuild >>= req
    performWithPb = performJSONRequestResponseAnnotatedWithHeaders @fe route headers . withPb

-- | We should apply this to lower level funcs such as performRequestAsync and make a PR into reflex-dom
-- |
-- | Mutually exclusive to when you would use runAPIPostBuild
runAPIResponseGated
  :: forall fe backendRoute cfg toJson fromJson t m err.
     ( ToJSON toJson
     , FromJSON err
     , FromJSON fromJson
     , Typeable fromJson
     , Typeable err
     , HasConfigs (Client m)
     , Prerender t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     , MonadReader cfg m
     )
  => R backendRoute
  -> Event t toJson
  -> m (Event t (RequestError err), Event t fromJson)
runAPIResponseGated route evPayload = mdo
  shouldFire <- holdDyn True $ leftmost [ False <$ evPayload , True <$ res, True <$ err ]
  (err, res) <- runAPI @fe route $ gate (current shouldFire ) evPayload
  pure (err,res)

-- | This probably should be deprecated in favor of performJSONRequestResponseAnnotated
performJSONRequestResponse
  :: forall fe backendRoute cfg toJson fromJson t m.
     ( HasConfigs m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     , ToJSON toJson
     , FromJSON fromJson
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     , MonadReader cfg m

     )
  => R backendRoute
  -> Event t toJson
  -> m (Event t (Either ErrorRead fromJson))
performJSONRequestResponse route jsonEv = do
  (fmap . fmap) decodeXhrResponse' $ performJSONRequest @fe route jsonEv

-- | Includes a timeout
-- | Updates read error to look like haskell compiler type error
-- | TODO: send this error to the Server to be emailed and logged
-- | TODO: timing out -> Request_ErrorRead
performJSONRequestResponseAnnotated
  :: forall fe backendRoute cfg t m toJson fromJson.
     ( HasConfigs m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     , ToJSON toJson
     , FromJSON fromJson
     , Typeable fromJson
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     , MonadReader cfg m
     )
  => R backendRoute
  -> Event t toJson
  -> m (Event t (Either T.Text fromJson))
performJSONRequestResponseAnnotated route jsonEv =
  performJSONRequestResponseAnnotatedWithHeaders @fe route mempty jsonEv
  -- do
  -- evXhrResponse :: Event t XhrResponse <- performJSONRequest route jsonEv
  -- routeText <- (renderAceAPI route)
  -- pure $ leftmost
  --   [ decodeXhrAnnotate routeText <$> evXhrResponse
  --   ]

performJSONRequestResponseAnnotatedWithHeaders
  :: forall fe backendRoute cfg t m toJson fromJson.
     ( HasConfigs m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     , ToJSON toJson
     , FromJSON fromJson
     , Typeable fromJson
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     , MonadReader cfg m
     )
  => R backendRoute
  -> Map.Map T.Text T.Text
  -> Event t toJson
  -> m (Event t (Either T.Text fromJson))
performJSONRequestResponseAnnotatedWithHeaders route headers jsonEv = do
  evXhrResponse :: Event t XhrResponse <- performJSONRequestWithHeaders @fe route headers jsonEv
  routeLink <- renderFullRouteBE @fe route
  pure $ leftmost
    [ decodeXhrAnnotate (getLink routeLink) <$> evXhrResponse
    ]

decodeXhrAnnotate
  :: forall fromJson.
  ( Typeable fromJson
  , FromJSON fromJson
  ) => T.Text -> XhrResponse -> Either T.Text fromJson
decodeXhrAnnotate routeString xhr =
  let
    annotate e = annotateError
      routeString
      (fromMaybe "" $ _xhrResponse_responseText xhr)
      (T.pack . show $ typeRep (Proxy :: Proxy fromJson))
      e
    mapLeft :: (a -> b) -> Either a c -> Either b c
    mapLeft = flip bimap Prelude.id
  in
    mapLeft annotate . decodeXhrResponse' $ xhr



annotateError :: T.Text -> T.Text -> T.Text -> String -> T.Text
annotateError routeStr actualBody expectedType baseError =
  "Request Error from: {"
  <> routeStr
  <> "} Expected("
  <> expectedType
  <> ") but Received("
  <> determineActual actualBody
  <> ")"
  <> "During base error:"
  <> T.pack baseError
  where
    determineActual jsonString =
      case Aeson.decode (LBS.fromStrict . T.encodeUtf8 $ jsonString)  :: Maybe Aeson.Value of
        Nothing -> jsonString
        Just val_ -> case val_ of
          Array _  -> "Array"
          String _ -> "String"
          Number _ -> "Number"
          Bool _   -> "Bool"
          Null     -> "Null"
          Aeson.Object keyMap_ -> case Aeson.lookup (Aeson.fromString "Right") keyMap_ of
            -- note that we can assume left will work because even if it was encoded with another type for Right
            -- that evidence doesn't exist in the string
            Nothing -> T.pack . show $ keyMap_
            Just rightCase -> case rightCase of
              Aeson.Object keyMap__ ->
                let
                  parser :: Stream s Identity Char => Parsec s u String
                  parser = char '_' *> some alphaNum <* char '_'
                  titlize s_ = T.toUpper (T.take 1 s_) <> (T.drop 1 s_)
                in
                  case Aeson.keys keyMap__ of
                    [] -> T.pack . show $ keyMap__
                    (k:_) -> case parse parser "" (Aeson.toString k) of
                      Left _ -> "An Object With Key:" <> Aeson.toText k
                      Right parent -> titlize . T.pack $ parent
              _ -> T.pack . show $ rightCase


showType :: forall a. Typeable a => a -> String
showType _ = show $ typeRep (Proxy :: Proxy a)


performJSONRequestWithHeaders
  :: forall fe backendRoute json cfg t m.
     ( MonadJSM (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     , ToJSON json
     , HasConfigs m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     , MonadReader cfg m
     )
  => R backendRoute
  -> Map.Map T.Text T.Text
  -> Event t json
  -> m (Event t XhrResponse)
performJSONRequestWithHeaders route headers jsonEv = do
  routeText <- (renderFullRouteBE @fe route)
  performRequestAsync $ withHeaders headers . withCred . postJson (getLink routeText) <$> jsonEv


performJSONRequest
  :: forall fe backendRoute json cfg t m.
     ( MonadJSM (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     , ToJSON json
     , HasConfigs m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     , MonadReader cfg m
     )
  => R backendRoute
  -> Event t json
  -> m (Event t XhrResponse)
performJSONRequest route jsonEv = do
  routeText <- (renderFullRouteBE @fe route)
  performRequestAsync $ withCred <$> postJson (getLink routeText) <$> jsonEv

withCred :: XhrRequest a -> XhrRequest a
withCred xhr = xhr & xhrRequest_config . xhrRequestConfig_withCredentials .~ True

withHeaders :: Map.Map T.Text T.Text -> XhrRequest a -> XhrRequest a
withHeaders headers xhr = xhr & xhrRequest_config . xhrRequestConfig_headers .~ headers

toEith :: Maybe a -> Either T.Text a
toEith = \case { Just a -> Right a ; Nothing -> Left "unable to parse response, please report this error" }


runRequest :: ( Prerender t m
              , Applicative m
              ) => Client m (Event t r) -> m (Event t r)
runRequest req = fmap switchDyn $ prerender (pure never) ( comment "runRequest:Hydrated" >> req )


type ApiBE e a = Either (BackendError e) a

fanResponse
  :: Reflex t
  => Event t (Either ErrorRead (Either (BackendError e) a))
  -> (Event t ErrorRead, Event t (BackendError e), Event t a)
fanResponse res =
  let
    (errRead, apiResult) = fanEither res
    (errApi, good) = fanEither apiResult
  in (errRead, errApi, good)

fanResponse'
  :: Reflex t
  => Event t (Either T.Text (Either (BackendError e) a))
  -> (Event t (RequestError e), Event t a)
fanResponse' res =
  let
    (errRead, apiResult) = fanEither res
    (errApi, good) = fanEither apiResult
    err = leftmost [ Request_ErrorAPI <$> errApi
                   , Request_ErrorRead <$> errRead
                   ]
  in
    (err, good)
