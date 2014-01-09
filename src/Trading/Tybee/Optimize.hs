module Trading.Tybee.Optimize (

) where

import qualified Math.Optimization.SPSA as SPSA

import Numeric.LinearAlgebra (Vector)

import Trading.Tybee.Base (OptimizableTradingStrategy(..), Shares(..), History(..), VariableRepr(..), Allocation(..), valueAtPrice)
import Trading.Tybee.Strategy (TybeeStrategy(..))
import Trading.Tybee.Simulation (simulate)

-----------