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
import Signal (Channel, Signal, memoSignal, newChannel, newState, readSignal, subscribe, writeChannel)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.Event.Internal.Types (EventTarget)

class MonadEffect m <= MonadHooks m where
  -- | Add a cleaner
  useCleaner :: Effect Unit -> m Unit
  -- | Unwrap a Signal
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

-- | Void version of `useHooks`
useHooks_ :: forall m a. MonadHooks m => Signal (m a) -> m Unit
useHooks_ sig = void $ useHooks sig

-- | Conditionally switch Signal values.
useIf :: forall m a. MonadHooks m => Signal Boolean -> m a -> m a -> m (Signal a)
useIf cond ifTrue ifFalse = useHooks $ cond <#> \c -> if c then ifTrue else ifFalse

-- | Void version of `useIf`
useIf_ :: forall m a. MonadHooks m => Signal Boolean -> m a -> m a -> m Unit
useIf_ cond ifTrue ifFalse = void $ useIf cond ifTrue ifFalse

-- | `Just a` when `cond` is `true`, `Nothing` otherwise.
useWhen :: forall m a. MonadHooks m => Signal Boolean -> m a -> m (Signal (Maybe a))
useWhen cond ifTrue = useIf cond (Just <$> ifTrue) (pure Nothing)

-- | Void version of `useWhen`
useWhen_ :: forall m a. MonadHooks m => Signal Boolean -> m a -> m Unit
useWhen_ cond ifTrue = void $ useWhen cond ifTrue

-- | Unwrap effective Signal
useEffect :: forall m a. MonadHooks m => Signal (Effect a) -> m (Signal a)
useEffect sig = useHooks $ sig <#> liftEffect

-- | Void version of `useEffect`
useEffect_ :: forall m a. MonadHooks m => Signal (Effect a) -> m Unit
useEffect_ = void <<< useEffect

-- | Unwrap Aff Signal.
-- | If the order of the results is reversed, it is ignored.
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

-- | Void version of `useAff`
useAff_ :: forall m a. MonadHooks m => Signal (Aff a) -> m Unit
useAff_ sig = useEffect_ $ sig <#> \aff -> launchAff_ $ void aff

-- | Subscribe to some Events
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

-- | Subscribe to interval events.
useInterval :: forall m. MonadHooks m => Int -> m Unit -> m Unit
useInterval ms handler = do
  let
    subscribe callback = do
      interval <- liftEffect $ setInterval ms $ callback unit
      pure $ clearInterval interval
  useSubscriber subscribe $ const handler

-- | Subscribe to timeout events.
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

-- | Run a `Hooks` computation and return the result and a cleanup effect.
runHooks :: forall m a. MonadEffect m => Hooks a -> m (Tuple a (Effect Unit))
runHooks (Hooks m) = liftEffect $ runWriterT m

-- | Void version of `runHooks`.
runHooks_ :: forall a. Hooks a -> Effect Unit
runHooks_ m = void $ runHooks m

-- | Lift `Hooks` to `m` which has `MonadHooks` instance.
liftHooks :: forall m a. MonadHooks m => Hooks a -> m a
liftHooks m = do
  Tuple a cln <- runHooks m
  useCleaner cln
  pure a

-- | Nub a Eq value of Signal.
nubEq :: forall m a. MonadHooks m => Eq a => Signal a -> m (Signal a)
nubEq sig = do
  chn <- newChannel $ unsafeCoerce unit
  useHooks_ $ sig <#> \a -> do
    prev <- readSignal $ subscribe chn
    unless (a == prev) $ writeChannel chn a
  pure $ subscribe chn

newStateEq :: forall m a. MonadHooks m => Eq a => a -> m (Tuple (Signal a) (Channel a))
newStateEq a = do
  Tuple sig chn <- newState a
  sig' <- nubEq sig
  pure $ Tuple sig' chn
