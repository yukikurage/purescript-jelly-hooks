module Signal.Hooks where

import Prelude

import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
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

class MonadEffect m <= MonadHooks r m | m -> r where
  liftHooks :: forall a. Hooks r a -> m a

instance (MonadHooks r m, Monoid w) => MonadHooks r (WriterT w m) where
  liftHooks = lift <<< liftHooks

instance MonadHooks r m => MonadHooks r (ReaderT r' m) where
  liftHooks = lift <<< liftHooks

instance MonadHooks r m => MonadHooks r (StateT s m) where
  liftHooks = lift <<< liftHooks

newtype Hooks r a = Hooks (ReaderT (Record r) (WriterT (Effect Unit) Effect) a)

derive newtype instance Functor (Hooks r)
derive newtype instance Apply (Hooks r)
derive newtype instance Applicative (Hooks r)
derive newtype instance Bind (Hooks r)
derive newtype instance Monad (Hooks r)
derive newtype instance MonadEffect (Hooks r)
derive newtype instance MonadRec (Hooks r)
instance MonadHooks r (Hooks r) where
  liftHooks = identity

runHooks :: forall r m a. MonadEffect m => Hooks r a -> Record r -> m (Tuple a (Effect Unit))
runHooks (Hooks m) r = liftEffect $ runWriterT (runReaderT m r)

runHooks_ :: forall r a. Hooks r a -> Record r -> Effect a
runHooks_ m r = do
  Tuple a _ <- runHooks m r
  pure a

useCleaner :: forall r m. MonadHooks r m => Effect Unit -> m Unit
useCleaner cleaner = liftHooks $ Hooks $ tell cleaner

useContext :: forall r m. MonadHooks r m => m (Record r)
useContext = liftHooks $ Hooks ask

useHooks :: forall r m a. MonadHooks r m => Signal (Hooks r a) -> m (Signal a)
useHooks sig = do
  context <- useContext
  Tuple res cln <- memoSignal $ sig <#> \h -> runHooks h context
  useCleaner cln
  pure res

useHooks_ :: forall r m a. MonadHooks r m => Signal (Hooks r a) -> m Unit
useHooks_ sig = do
  context <- useContext
  let
    sig' = sig <#> \h -> do
      Tuple _ cleaner <- runHooks h context
      pure cleaner
  useCleaner =<< runSignal sig'

useEffect :: forall r m a. MonadHooks r m => Signal (Effect a) -> m (Signal a)
useEffect sig = useHooks $ sig <#> liftEffect

useEffect_ :: forall r m a. MonadHooks r m => Signal (Effect a) -> m Unit
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
