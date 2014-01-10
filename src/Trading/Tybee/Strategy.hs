module Trading.Tybee.Strategy (
  TybeeStrategy(..)

) where

import Control.Monad.State (State,get,put)

import Trading.Tybee.Base (
  TradingStrategy(..),
  Allocation(..), valueAtPrice,
  Price(..), avg,
  Action(..), isSell,
  History,
  Shares(..),
  VariableRepr(..),
  OptimizableTradingStrategy(..)
  )

import Trading.Tybee.Simulation (simulate)


-- | A simple strategy with buy, sell, and stoploss points based on current price (at calculation)
data TybeeStrategy s p = TybeeStrategy { currentPrice, buyMod, sellMod, stopMod :: p } deriving (Show)

instance (Shares s p) => TradingStrategy TybeeStrategy s p where
  initStrategy prc strat = strat { currentPrice = avg prc }
  trade = tradeTybee

tradeTybee :: (Shares s p) => Price p -> State (TybeeStrategy s p, Allocation s p) (Action s)
tradeTybee price = do
  (tybee, alloc) <- get
  action <- return $ decideTybee alloc price tybee
  if isSell action then put (updateCurrentPrice price tybee, alloc) else return ()
  return action

decideTybee :: (Shares s p) => Allocation s p -> Price p -> TybeeStrategy s p -> Action s
decideTybee alloc price tybee = case ((currency alloc) > 0 && (shouldBuy price tybee), (shares alloc) > 0 && (shouldSell price tybee)) of
    (True, _) -> Buy (toShares (ask price) (currency alloc))
    (False, True) -> Sell (shares alloc)
    (False, False) -> Hold


instance Num p => VariableRepr (TybeeStrategy s p) p where
  toVariables strat = [fn strat | fn <- [buyMod, sellMod, stopMod]]
  fromVariables [b,s,st] = TybeeStrategy { currentPrice = 0, buyMod = b, sellMod = s, stopMod = st }

instance (Shares s p) => OptimizableTradingStrategy TybeeStrategy s p where
  strategyConstrain = constrainTybee
  strategyLoss hist strat = - (simulateTybee hist strat)

constrainTybee :: (Shares s p) => TybeeStrategy s p -> TybeeStrategy s p
constrainTybee (TybeeStrategy {buyMod=b,sellMod=s,stopMod=st}) = let b' = (minimax b (1/tybeeBound) tybeeBound) in
  TybeeStrategy {
    currentPrice = 0,
    buyMod = b',
    sellMod = (minimax s b' tybeeBound),
    stopMod = (minimax st (1/tybeeBound) b')
    }

simulateTybee :: (Shares s p) => History p -> TybeeStrategy s p -> p
simulateTybee hist strat = let alloc = simulate initAlloc hist strat in valueAtPrice (last hist) alloc

-----------
-- Helpers
-----------

updateCurrentPrice :: (Shares s p) => Price p -> TybeeStrategy s p -> TybeeStrategy s p
updateCurrentPrice price tybee = tybee {currentPrice = avg price}

buyPrice :: Shares s p => TybeeStrategy s p -> p
buyPrice (TybeeStrategy {currentPrice = cur, buyMod = m}) = cur * m

sellPrice :: Shares s p => TybeeStrategy s p -> p
sellPrice (TybeeStrategy {currentPrice = cur, sellMod = m}) = cur * m

stopPrice :: Shares s p => TybeeStrategy s p -> p
stopPrice (TybeeStrategy {currentPrice = cur, stopMod = m}) = cur * m

shouldBuy :: Shares s p => Price p -> TybeeStrategy s p -> Bool
shouldBuy price tybee = (buyPrice tybee) >= (ask price)

shouldSell :: Shares s p => Price p -> TybeeStrategy s p -> Bool
shouldSell price tybee = (sellPrice tybee) <= (bid price) || (stopPrice tybee) >= (bid price)

tybeeBound :: (Num f, Floating f) => f
tybeeBound = 2

minimax :: (Ord f) => f -> f -> f -> f
minimax x y z = (min z . max y) x

initAlloc :: Shares s p => Allocation s p
initAlloc = Allocation { shares = 0, currency = 1000 }
