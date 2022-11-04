module Test.Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Signal (writeChannel)
import Signal.Hooks (newStateEq, runHooks_, useHooks_)

main :: Effect Unit
main = runHooks_ do
  Tuple nSig nChn <- newStateEq 0
  useHooks_ $ nSig <#> \n -> do
    log $ "n = " <> show n
  writeChannel nChn 1
  writeChannel nChn 2
  writeChannel nChn 2
  writeChannel nChn 3
  writeChannel nChn 3
  writeChannel nChn 1
