module SodiumFRP.Transaction (
    runTransaction
) where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

runTransaction :: forall a. Effect a -> Effect a
runTransaction = runEffectFn1 runTransactionImpl 

foreign import runTransactionImpl :: forall a. EffectFn1 (Effect a) (a)
