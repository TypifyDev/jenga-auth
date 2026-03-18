{-# LANGUAGE TupleSections #-}

module Jenga.Frontend.TimeExtras where

import Jenga.Frontend.JS
import Reflex.Dom.Core
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Data.Time

timer :: ( PerformEvent t m
         , MonadIO (Performable m)
         , TriggerEvent t m
         , MonadFix m
         , MonadHold t m
         )
      => Event t ()
      -> Event t ()
      -> Event t ()
      -> m (Dynamic t NominalDiffTime)
timer start stop reset = do
  startTimeEv <- performEvent $ liftIO getCurrentTime <$ start
  mStartTime <- holdDyn Nothing $ leftmost
    [ Just <$> startTimeEv
    , Nothing <$ stop
    , Nothing <$ reset
    ]
  -- Single ticker: only seed from the FIRST start event to avoid accumulation
  firstStart <- headE startTimeEv
  tick <- tickLossyFrom' $ (0.1,) <$> firstStart
  let elapsed = attachWith (\mStart tickInfo ->
        case mStart of
          Nothing -> 0
          Just s  -> diffUTCTime (_tickInfo_lastUTC tickInfo) s
        ) (current mStartTime) tick
  holdDyn 0 elapsed

tickLossyFrom'' :: ( PerformEvent t m
                   , MonadIO (Performable m)
                   , TriggerEvent t m
                   , MonadFix m
                   ) => NominalDiffTime -> Event t a -> m (Event t TickInfo)
tickLossyFrom'' nomnom ev = do
  eventTime <- performEvent $ liftIO getCurrentTime <$ ev
  tickLossyFrom' $ (nomnom,) <$> eventTime

countTimeFrom :: ( PerformEvent t m
                 , MonadIO (Performable m)
                 , TriggerEvent t m
                 , MonadFix m
                 , MonadHold t m
                 ) => NominalDiffTime -> Event t a -> m (Event t NominalDiffTime)
countTimeFrom interval ev = do
  eventTime <- performEvent $ liftIO getCurrentTime <$ ev
  eventTimeDyn <- foldDyn const undefined {-never evals, this fires once-} eventTime
  tick <- tickLossyFrom' $ (interval,) <$> eventTime
  pure $ attachWith (\start now_ -> diffUTCTime (_tickInfo_lastUTC now_) start) (current eventTimeDyn) tick


timeoutEvent
  :: ( PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , Trace t m
     )
  => NominalDiffTime
  -> Event t a
  -> Event t b
  -> m (Event t ())
timeoutEvent timeAllowed waitingFor start = do
  timeoutCond <- delay timeAllowed start
  timeoutRan <- holdDyn False $ True <$ timeoutCond
  -- todo: use start == waitingFor +1 not 0
  (sC :: Dynamic t Int, wC :: Dynamic t Int) <- (,) <$> count start <*> count waitingFor
  let hasResponded = (==) <$> sC <*> wC
  traceDyn' hasResponded
  pure $ fmap (const ()) $ ffilter id $ leftmost
    [ gate (not <$> current timeoutRan) $ False <$ waitingFor
    , gate (not <$> current hasResponded) $ True <$ timeoutCond
    ]

-- | Check if some amount of time has elapsed
-- | unlike delay in that delay automatically fires and would need to be stopped
-- | This also allows for flexibility on how the Dynamic t NominalDiffTime was created
-- | TODO upstream to Ace reflex fork
-- | TODO enable sampling interval
hasBeen :: Reflex t => NominalDiffTime -> Dynamic t NominalDiffTime -> Event t ()
hasBeen threshold tElapsed = mapMaybe (\t_ -> if t_ > threshold then Just () else Nothing) (updated tElapsed)

hasBeenRange :: Reflex t => (NominalDiffTime, NominalDiffTime) -> Dynamic t NominalDiffTime -> Event t ()
hasBeenRange (low, high) tElapsed = mapMaybe (\t_ -> if t_ > low && t_ < high then Just () else Nothing) (updated tElapsed)

getPostBuildDelayed
  :: ( PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , MonadHold t m
     )
     -- renderTimes :: [NominalDiffTime]
-- renderTimes = [0.0001, 0.5, 1.0, 2.0, 5.0 , 7.0 , 10.0, 20.0 ]
  => [NominalDiffTime]
  -> m (Event t ())
getPostBuildDelayed renderTimes = do
  pBuild <- getPostBuild
  -- At ten seconds a timeout error is thrown so only until 9 is necessary
  pbDs <- forM renderTimes $ \t_ -> do
    delay t_ pBuild
  fmap (() <$) $ headE $ leftmost pbDs
  --pure $ () <$ x
