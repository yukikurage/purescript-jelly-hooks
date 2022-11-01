module Signal.Hooks where

import Prelude

import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.State (StateT, state)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Signal (Signal, readSignal, runSignal, send, signal)

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
instance MonadClean (Hooks r) where
  useCleaner cleaner = Hooks $ tell cleaner

instance MonadContext r (Hooks r) where
  useContext = Hooks ask

runHooks :: forall r a. Hooks r a -> Record r -> Effect (Tuple a (Effect Unit))
runHooks (Hooks m) r = runWriterT (runReaderT m r)

useHooks
  :: forall r m
   . MonadHooks r m
  => Signal (Hooks r Unit)
  -> m Unit
useHooks sig = do
  context <- useContext
  let
    sig' = sig <#> \hooks -> do
      Tuple _ cleaner <- runHooks hooks context
      pure cleaner
  useCleaner =<< runSignal sig'

-- useMemo
--   :: forall r m a
--    . MonadHooks r m
--   => Signal (Hooks r a)
--   -> m (Signal a)
-- useMemo sig = do
--   Tuple stateSig stateChn <- signal =<< readSignal sig
--   useHooks $ sig <#> \hooks -> do
--     a <- hooks
--     send stateChn a