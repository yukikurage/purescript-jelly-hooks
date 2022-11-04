module Signal.Hooks where

import Prelude

import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (modify, new, read, write)
import Effect.Timer (clearInterval, clearTimeout, setInterval, setTimeout)
import Signal (Signal, memoSignal, newState, writeChannel)
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.Event.Internal.Types (EventTarget)

class MonadEffect m <= MonadHooks m where
  useCleaner :: Effect Unit -> m Unit
  useHooks :: forall a. Signal (m a) -> m (Signal a)

instance MonadHooks m => MonadHooks (ReaderT r m) where
  useCleaner = lift <<< useCleaner
  useHooks sig = do
    r <- ask
    lift $ useHooks $ flip runReaderT r <$> sig

instance (MonadHooks m, Monoid w) => MonadHooks (WriterT (Signal w) m) where
  useCleaner = lift <<< useCleaner
  -- Maybe a little non-trivial implementation.
  useHooks sig = do
    sigAW <- lift $ useHooks $ runWriterT <$> sig
    tell $ join $ snd <$> sigAW
    pure $ fst <$> sigAW

useHooks_ :: forall m a. MonadHooks m => Signal (m a) -> m Unit
useHooks_ sig = void $ useHooks sig

useIf :: forall m a. MonadHooks m => Signal Boolean -> m a -> m a -> m (Signal a)
useIf cond ifTrue ifFalse = useHooks $ cond <#> \c -> if c then ifTrue else ifFalse

useIf_ :: forall m a. MonadHooks m => Signal Boolean -> m a -> m a -> m Unit
useIf_ cond ifTrue ifFalse = void $ useIf cond ifTrue ifFalse

useWhen :: forall m a. MonadHooks m => Signal Boolean -> m a -> m (Signal (Maybe a))
useWhen cond ifTrue = useIf cond (Just <$> ifTrue) (pure Nothing)

useWhen_ :: forall m a. MonadHooks m => Signal Boolean -> m a -> m Unit
useWhen_ cond ifTrue = void $ useWhen cond ifTrue

useEffect :: forall m a. MonadHooks m => Signal (Effect a) -> m (Signal a)
useEffect sig = useHooks $ sig <#> liftEffect

useEffect_ :: forall m a. MonadHooks m => Signal (Effect a) -> m Unit
useEffect_ = void <<< useEffect

useAff :: forall m a. MonadHooks m => Signal (Aff a) -> m (Signal (Maybe a))
useAff sig = do
  currentRef <- liftEffect $ new 0
  Tuple resSig chn <- newState Nothing
  let
    sig' = sig <#> \aff -> do
      current <- liftEffect $ modify (_ + 1) currentRef
      launchAff_ do
        a <- aff
        current' <- liftEffect $ read currentRef
        when (current == current') $ writeChannel chn $ Just a
  useEffect_ sig'
  pure resSig

useAff_ :: forall m a. MonadHooks m => Signal (Aff a) -> m Unit
useAff_ sig = useEffect_ $ sig <#> \aff -> launchAff_ $ void aff

useSubscriber :: forall m e. MonadHooks m => ((e -> Effect Unit) -> Effect (Effect Unit)) -> (e -> m Unit) -> m Unit
useSubscriber subscribe handler = do
  Tuple sig chn <- newState $ pure unit
  sub <- liftEffect $ subscribe \e -> writeChannel chn $ handler e *> pure unit
  useCleaner sub
  useHooks_ sig

-- | Subscribe to an event on an event target.
useEvent :: forall m. MonadHooks m => EventTarget -> EventType -> (Event -> m Unit) -> m Unit
useEvent target eventType handler = do
  let
    subscribe callback = do
      el <- liftEffect $ eventListener callback
      liftEffect $ addEventListener eventType el false target
      pure $ removeEventListener eventType el false target
  useSubscriber subscribe handler

-- | A hook that runs an effect every `n` milliseconds.
useInterval :: forall m. MonadHooks m => Int -> m Unit -> m Unit
useInterval ms handler = do
  let
    subscribe callback = do
      interval <- liftEffect $ setInterval ms $ callback unit
      pure $ clearInterval interval
  useSubscriber subscribe $ const handler

-- | A hook that runs a handler after a given number of milliseconds.
useTimeout :: forall m. MonadHooks m => Int -> m Unit -> m Unit
useTimeout ms handler = do
  let
    subscribe callback = do
      timeout <- liftEffect $ setTimeout ms $ callback unit
      pure $ clearTimeout timeout
  useSubscriber subscribe $ const handler

-- | A hook that runs the given effect when the signal changes. (without initialize)
useUpdate :: forall m. MonadHooks m => Signal (m Unit) -> m Unit
useUpdate sig = do
  isInit <- liftEffect $ new true
  useHooks_ $ sig <#> \eff -> do
    init <- liftEffect $ read isInit
    if init then liftEffect $ write false isInit *> mempty else eff

newtype Hooks a = Hooks (WriterT (Effect Unit) Effect a)

derive newtype instance Functor Hooks
derive newtype instance Apply Hooks
derive newtype instance Applicative Hooks
derive newtype instance Bind Hooks
derive newtype instance Monad Hooks
derive newtype instance MonadEffect Hooks
derive newtype instance MonadRec Hooks
instance MonadHooks Hooks where
  useCleaner cleaner = Hooks $ tell cleaner
  useHooks sig = do
    Tuple res cln <- memoSignal $ sig <#> \h -> runHooks h
    Hooks $ tell cln
    pure res

runHooks :: forall m a. MonadEffect m => Hooks a -> m (Tuple a (Effect Unit))
runHooks (Hooks m) = liftEffect $ runWriterT m

runHooks_ :: forall a. Hooks a -> Effect a
runHooks_ m = do
  Tuple a _ <- runHooks m
  pure a

liftHooks :: forall m a. MonadHooks m => Hooks a -> m a
liftHooks m = do
  Tuple a cln <- runHooks m
  useCleaner cln
  pure a
