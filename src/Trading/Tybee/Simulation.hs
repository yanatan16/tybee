module Trading.Tybee.Simulation (
  simulate
) where

import Control.Monad.State (State,runState,MonadState(..))

import Trading.Tybee.Base (Price(..), History, Action(..), Allocation(..), buyAtPrice, sellAtPrice, TradingStrategy(..))


simulate :: TradingStrategy a s p => Allocation s p -> History p -> a s p -> Allocation s p
simulate alloc hist str = snd $ foldl doTrade (str, alloc) hist
  where
    doTrade st (_, p) = snd $ runState (simulateSlice p) st

simulateSlice :: TradingStrategy a s p => Price p -> State (a s p, Allocation s p) ()
simulateSlice prc = do
  action <- trade prc
  (strat, alloc) <- get
  case action of
    Hold -> return ()
    Buy b -> put (strat, buyAtPrice b (ask prc) alloc)
    Sell b -> put (strat, sellAtPrice b (bid prc) alloc)