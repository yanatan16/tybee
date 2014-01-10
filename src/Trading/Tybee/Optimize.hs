module Trading.Tybee.Optimize (
  optimizeTybee
) where

import qualified Math.Optimization.SPSA as SPSA

import Numeric.LinearAlgebra (Vector,toList,fromList)

import Trading.Tybee.Base (OptimizableTradingStrategy(..), History, VariableRepr(..))
import Trading.Tybee.Strategy

-----------

toStrategy :: (OptimizableTradingStrategy a s Double) => Vector Double -> a s Double
toStrategy = fromVariables . toList

fromStrategy :: (OptimizableTradingStrategy a s Double) => a s Double -> Vector Double
fromStrategy = fromList . toVariables

-- I couldn't figure out a way to not define these not for just tybee, especially with loss . fromVector and such

tybeeLoss :: History Double -> TybeeStrategy Int Double -> Double
tybeeLoss = strategyLoss

tybeeConstrain :: TybeeStrategy Int Double -> TybeeStrategy Int Double
tybeeConstrain = strategyConstrain

optimizeTybee :: History Double -> TybeeStrategy Int Double -> IO (TybeeStrategy Int Double)
optimizeTybee hist strat = do
  (ak,ck) <- return $ SPSA.semiautomaticTuning 1 0.1
  delta <- SPSA.bernoulli (length $ (toVariables strat :: [Double]))
  spsa <- return SPSA.SPSA{
    SPSA.loss = (tybeeLoss hist) . toStrategy,
    SPSA.constraint = fromStrategy . tybeeConstrain . toStrategy,
    SPSA.ak = ak, SPSA.ck = ck,
    SPSA.delta = delta
    }
  return $ (toStrategy . (SPSA.optimize spsa 1000) . fromStrategy) strat