module Signal.Hooks where

import Prelude

import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (modify, new, read)
import Effect.Timer (clearInterval, clearTimeout, setInterval, setTimeout)
import Signal (Signal, memoSignal, newState, runSignal, writeChannel)
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.Event.Internal.Types (EventTarget)

class Monad m <= MonadClean m where
  useCleaner :: Effect Unit -> m Unit

instance (MonadClean m, Monoid w) => MonadClean (WriterT w m) where
  useCleaner = lift <<< useCleaner

instance MonadClean m => MonadClean (ReaderT r m) where
  useCleaner = lift <<< useCleaner

instance MonadClean m => MonadClean (StateT s m) where
  useCleaner = lift <<< useCleaner

class Monad m <= MonadContext r m | m -> r where
  useContext :: m (Record r)

instance (MonadContext r m, Monoid w) => MonadContext r (WriterT w m) where
  useContext = lift useContext

instance MonadContext r m => MonadContext r (ReaderT r' m) where
  useContext = lift useContext

instance MonadContext r m => MonadContext r (StateT s m) where
  useContext = lift useContext

class (MonadContext r m, MonadEffect m, MonadClean m) <= MonadHooks r m | m -> r

newtype Hooks r a = Hooks (ReaderT (Record r) (WriterT (Effect Unit) Effect) a)

derive newtype instance Functor (Hooks r)
derive newtype instance Apply (Hooks r)
derive newtype instance Applicative (Hooks r)
derive newtype instance Bind (Hooks r)
derive newtype instance Monad (Hooks r)
derive newtype instance MonadEffect (Hooks r)
instance MonadClean (Hooks r) where
  useCleaner cleaner = Hooks $ tell cleaner

instance MonadContext r (Hooks r) where
  useContext = Hooks ask

runHooks :: forall r m a. MonadEffect m => Hooks r a -> Record r -> m (Tuple a (Effect Unit))
runHooks (Hooks m) r = liftEffect $ runWriterT (runReaderT m r)

runHooks_ :: forall r a. Hooks r a -> Record r -> Effect a
runHooks_ m r = do
  Tuple a _ <- runHooks m r
  pure a

useHooks
  :: forall r m a
   . MonadHooks r m
  => Signal (Hooks r a)
  -> m (Signal a)
useHooks sig = do
  context <- useContext
  Tuple res cln <- memoSignal $ sig <#> \h -> runHooks h context
  useCleaner cln
  pure res

useHooks_
  :: forall r m a
   . MonadHooks r m
  => Signal (Hooks r a)
  -> m Unit
useHooks_ sig = do
  context <- useContext
  let
    sig' = sig <#> \h -> do
      Tuple _ cleaner <- runHooks h context
      pure cleaner
  useCleaner =<< runSignal sig'

useEffect
  :: forall r m a
   . MonadHooks r m
  => Signal (Effect a)
  -> m (Signal a)
useEffect sig = useHooks $ sig <#> liftEffect

useEffect_
  :: forall r m a
   . MonadHooks r m
  => Signal (Effect a)
  -> m Unit
useEffect_ = void <<< useEffect

useAff :: forall r m a. MonadHooks r m => Signal (Aff a) -> m (Signal (Maybe a))
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

useAff_ :: forall r m a. MonadHooks r m => Signal (Aff a) -> m Unit
useAff_ sig = useEffect_ $ sig <#> \aff -> launchAff_ $ void aff

useSubscriber :: forall r m e. MonadHooks r m => ((e -> Effect Unit) -> Effect (Effect Unit)) -> (e -> Hooks r Unit) -> m Unit
useSubscriber subscribe handler = do
  Tuple sig chn <- newState $ pure unit
  sub <- liftEffect $ subscribe \e -> writeChannel chn $ handler e *> pure unit
  useCleaner sub
  useHooks_ sig

useEvent :: forall r m. MonadHooks r m => EventTarget -> EventType -> (Event -> Hooks r Unit) -> m Unit
useEvent target eventType handler = do
  let
    subscribe callback = do
      el <- liftEffect $ eventListener callback
      liftEffect $ addEventListener eventType el false target
      pure $ removeEventListener eventType el false target
  useSubscriber subscribe handler

useInterval :: forall r m. MonadHooks r m => Int -> Hooks r Unit -> m Unit
useInterval ms handler = do
  let
    subscribe callback = do
      interval <- liftEffect $ setInterval ms $ callback unit
      pure $ clearInterval interval
  useSubscriber subscribe $ const handler

useTimeout :: forall r m. MonadHooks r m => Int -> Hooks r Unit -> m Unit
useTimeout ms handler = do
  let
    subscribe callback = do
      timeout <- liftEffect $ setTimeout ms $ callback unit
      pure $ clearTimeout timeout
  useSubscriber subscribe $ const handler

hooks :: forall r m a. MonadHooks r m => Hooks r (m a) -> m a
hooks m = do
  context <- useContext
  Tuple a cln <- runHooks m context
  useCleaner cln
  a
