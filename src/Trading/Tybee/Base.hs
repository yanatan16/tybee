module Trading.Tybee.Base (
  Price(..), avg,
  History,
  Allocation(..), buyAtPrice, sellAtPrice,
  Action(..), isSell, isBuy, isHold,
  Shares(..),
  TradingStrategy(..),
  Vectorize(..)
) where

import Data.Time (UniversalTime(..))

import Control.Monad.State (State)

import Numeric.LinearAlgebra (Vector,Element)

---------------------
-- Types
---------------------

-- | Price of an asset
data Price d = Price { bid, ask :: d } deriving (Show, Eq)

instance (Num d, RealFloat d, Ord d) => Ord (Price d) where
  compare p1 p2 = compare (avg p1) (avg p2)

avg :: (Num d, RealFloat d) => Price d -> d
avg (Price{bid=b, ask=a}) = (b + a) / 2

-- | Price history of an asset
type History d = [(UniversalTime, Price d)]

-- | Shares are counted in type b and priced at type d
class (Num s, Ord s, Num p, Ord p, RealFloat p) => Shares s p where
  toPrice :: p -> s -> p
  toPrice = error "Not Implemented"

  toShares :: p -> p -> s
  toShares = error "Not Implemented"

instance Shares Double Double where
  toPrice prc shrs = prc * shrs
  toShares prc amt = amt / prc
instance Shares Float Float where
  toPrice prc shrs = prc * shrs
  toShares prc amt = amt / prc

instance (Integral s) => Shares s Double where
  toPrice prc shrs = prc * (fromIntegral shrs)
  toShares prc amt = floor $ amt / prc
instance (Integral s) => Shares s Float where
  toPrice prc shrs = prc * (fromIntegral shrs)
  toShares prc amt = floor $ amt / prc

-- | Amount allocated in the asset and amount available
data Allocation s p = Allocation { shares :: s, currency :: p } deriving (Show)

buyAtPrice :: Shares s p => s -> p -> Allocation s p -> Allocation s p
buyAtPrice shrs prc (Allocation {shares=s, currency=c}) = Allocation { shares = s + shrs, currency = c - (toPrice prc shrs) }

sellAtPrice :: Shares s p => s -> p -> Allocation s p -> Allocation s p
sellAtPrice shrs = buyAtPrice (-shrs)

-- | Action to take
data Action s = Buy s | Sell s | Hold

isSell :: Action s -> Bool
isSell (Sell _) = True
isSell _ = False

isBuy :: Action s -> Bool
isBuy (Buy _) = True
isBuy _ = False

isHold :: Action s -> Bool
isHold Hold = True
isHold _ = False

-- | Strategy for trading
class (Shares s p) => TradingStrategy a s p where
  -- | trade action at a given moment
  trade :: Price p -> State (a s p, Allocation s p) (Action s)
  trade = error "Not implemented"

class (Element d) => Vectorize a d where
  -- | Map to a vector
  toVector :: a d -> Vector d
  toVector = error "Not Implemented"

  -- | Map from a vector
  fromVector :: Vector d -> a d
  fromVector = error "Not Implemented"






