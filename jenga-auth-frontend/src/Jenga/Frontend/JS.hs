{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Jenga.Frontend.JS where

import Jenga.Common.HasJengaConfig
import Obelisk.Route
import Reflex.Dom.Core
import Language.Javascript.JSaddle
import qualified GHCJS.DOM.Types as GHCJS
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Reader
import qualified Data.Text as T

-- | Simplified interface to running JS and returning it as a wrapped value
-- | We use either here to deal with the initial case. We could have used Maybe instead
-- | however this is just to clarify the use of this function.
-- | It is also built in this way so that you must handle what happens in the Left case / before the JS has executed
runJS :: (MonadJSM jsm, Applicative m, Prerender t m, Client m ~ jsm)
      => jsm a
      -> m (Dynamic t (Either String a))
runJS jsStatement = prerender (pure $ Left "Couldn't get JS value from its context") (Right <$> jsStatement) -- (do { x <- js; pure $ Right x })

-- | Simplified interface to running JS and returning it as an event
-- | The output event will only fire when the JS has been executed, this doesn't mean it will have been successful
-- | as this is some javascript effect which can fail for various reasons. Therefore in complex cases, you should
-- | probably be returning an (Event t (Either c b)) that tells you what went wrong, based on your own code/some test
runJSWhen :: ( Applicative m
             , Prerender t m
             )
           => Event t a
           -> (a -> JSM b)
           -> m (Event t b)
runJSWhen event jsFunc = fmap switchDyn $ prerender (pure never) $ performEvent $ ffor event $ liftJSM . jsFunc

runJSWhenPb :: ( Prerender t m
               , PostBuild t m
               , PerformEvent t m
               , TriggerEvent t m
               , MonadIO (Performable m)
               , MonadHold t m
               )
            => (JSM b)
            -> m (Event t b)
runJSWhenPb jsStatement = do
  pBuild <- getPostBuild
  output <- runJSWhen pBuild $ \_ -> jsStatement
  headE output

runJSWhenRendered
  :: ( Prerender t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , MonadHold t m
     )
  => (JSM b)
  -> m (Event t b)
runJSWhenRendered jsStatement = do
  pBuild <- onRender
  output <- runJSWhen pBuild $ \_ -> jsStatement
  headE output

-- TODO move to reflex-dom-core and maybe explain why/when this is necessary
onRender :: (Prerender t m, Monad m) => m (Event t ())
onRender = fmap updated (prerender blank blank)

clog :: (MonadJSM m, ToJSVal a) => a -> m ()
clog a = do
  _ <- liftJSM $ jsg "console" ^. js1 "log" a
  pure ()

clogErr :: forall e m a. (ToJSVal e, MonadJSM m) => e -> m (Either e a)
clogErr e = liftJSM $ do
  _ <- liftJSM $ jsg "console" ^. js1 "error" e
  pure $ Left e

clogSend
  :: forall fe backendRoute cfg a m.
     ( MonadJSM m
     , ToJSVal a
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     , MonadReader cfg m
     )
  => R backendRoute
  -> a
  -> m ()
clogSend route a = do
  clog a
  logRoute <- renderFullRouteBE @fe route
  req <- postJson (getLink logRoute) . show <$> liftJSM (valToStr a)
  _ <- newXMLHttpRequestWithError req $ \err ->
    let
      showXhr = \case
        Left e -> show $ ( Left e :: Either XhrException String)
        Right res -> show $ ( Right $ _xhrResponse_responseText res  :: Either XhrException (Maybe T.Text))
    in
      clog $ showXhr err
  pure ()


-- | TODO: upstream to Reflex Dom
-- | Instead of HTMLRef -> use Element
-- |
-- | Add an HTML event listener that provides a notification in Haskell land when it fires
-- | Prior to this, it was hard to deal with setting specific events in a functional manner
-- | For example: This could be used to refactor how we get our blob from the Recorder which is
-- | put to an array we've held reference to, however this could simply fire the blob event as
-- | if it was some simple click
type HTMLRef = JSVal
type HTMLEventType = T.Text
type HTMLEvent = JSVal
--
emptyEvent :: HTMLEvent -> JSM ()
emptyEvent = pure . const ()
--
addHTMLEventListener ::
  ( TriggerEvent t m
  , Prerender t m
  )
  => HTMLRef
  -> HTMLEventType
  -> (HTMLEvent -> JSM a)
  -> m (Event t a)
addHTMLEventListener htmlRef htmlEventType actn = do
  (event, trigger) <- newTriggerEvent
  prerender_ blank $ void $ liftJSM $ htmlRef ^. js2 "addEventListener" htmlEventType
    (fun $ \_ _ e -> do
        out <- actn $ head e
        (liftIO $ trigger out)
        pure ()
    )
  pure event

-- | Figuring this out likely takes a great deal of understanding DomBuilder generalization via classes
-- | ; we know this works when { DomSpace d where d is GhcjsDomSpace } but not generically for DomSpace
-- |
addHTMLEventListener' ::
  ( TriggerEvent t m
  , MonadJSM m
  )
  => Element er GhcjsDomSpace t
  -> HTMLEventType
  -> (HTMLEvent -> JSM a)
  -> m (Event t a)
addHTMLEventListener' elementReflex htmlEventType actn = do
  (event, trigger) <- newTriggerEvent
  let raw = GHCJS.unElement . _element_raw $ elementReflex
  void $ liftJSM $ raw ^. js2 "addEventListener" htmlEventType
    (fun $ \_ _ e -> do
        out <- actn $ head e
        (liftIO $ trigger out)
        pure ()
    )
  pure event

type Trigger a = (a -> IO ())


-- | We give you the trigger to run (presumably) conditionally. If the trigger is not called, the resulting
-- | event == never :: Event t a
-- | Eg: \e trigger -> if e == _ then getThing >>= liftIO $ trigger else pure ()
addHTMLEventListener'' ::
  ( TriggerEvent t m
  , MonadJSM m
  )
  => Element er GhcjsDomSpace t
  -> HTMLEventType
  -> (HTMLEvent -> Trigger a -> JSM ())
  -> m (Event t a)
addHTMLEventListener'' elementReflex htmlEventType actn = do
  (event, trigger) <- newTriggerEvent
  let raw = GHCJS.unElement . _element_raw $ elementReflex
  void $ liftJSM $ raw ^. js2 "addEventListener" htmlEventType
    (fun $ \_ _ e -> do
        actn (head e) trigger
        pure ()
    )
  pure event

currentWindowUnchecked :: MonadJSM m => m JSVal -- no type equivalent for Window and Document
currentWindowUnchecked = liftJSM $ jsg "window"

currentDocumentUnchecked :: MonadJSM m => m GHCJS.Document
currentDocumentUnchecked = liftJSM $ fmap GHCJS.Document $ jsg "document"

traceDyn' :: (Trace t m, Show a) => Dynamic t a -> m ()
traceDyn' d = do
  let ev = updated d
  traceEvent' (T.pack "traceDyn") ev

type Trace t m = (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)

traceEvent' :: (Trace t m, Show a) => T.Text -> Event t a -> m ()
traceEvent' label ev = do
  el (T.pack "div") $ do
    el (T.pack "div") $ text label
    vals <- foldDyn (\a_ b_ -> a_ : b_) [] ev
    el (T.pack "div") $ dynText $ T.pack . show <$> vals
