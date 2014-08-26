{-# LANGUAGE DeriveDataTypeable #-}
module Vanilla.Types   
    ( 
     Input (..),    Deal (..),    DealInfo (..),    Portfolio (..), 
     Flex (..),     Product (..), Leg (..),         Coupon (..), 
     AddFlows (..), AddFlow (..), ValueStorage (..)
    ) where 

import Data.Time.Calendar
import Utils.MyJSON
import Utils.MyDates
import Vanilla.Models
import Vanilla.PayOffs
import Vanilla.ModelParameters
import Market.FinantialConventions
import Market.Indexes
import Market.MarketData
------------------------------------- BASIC TYPES ------------------------
-- input
data Input = Input {
                       inputDeal :: Deal, 
                       inputMarket :: MarketData, 
                       inputModelParams :: ModelParameters
                   } deriving (Eq, Show, Data, Typeable)  
-------------------------------------------------------------------------- 
-- Deal
data Deal = Deal {
                     dealInfo :: DealInfo, 
                     dealProduct :: Product
                 } deriving (Eq, Show, Data, Typeable)  
-------------------------------------------------------------------------- 
-- Deal information
data DealInfo = DealInfo {
                             portfolio :: String, 
                             evalDate :: Day, 
                             isPricingMode :: Bool, 
                             flex :: Flex
                         } deriving (Eq, Show, Data, Typeable)  
-------------------------------------------------------------------------- 
-- Flex info
data Flex = Flex {
                     flexType :: String, 
                     blockLabel :: String
                 } deriving (Eq, Show, Data, Typeable)  
-------------------------------------------------------------------------- 
-- Products             
data Product = Swap       {
                              swLeg1 :: Leg, 
                              swLeg2 :: Leg, 
                              addFlows :: AddFlows
                          }
             | CancelSwap {
                              cacelSwap :: Product, 
                              exerciseDates :: [Day]
                          } 
             | Option     {
                              opLeg :: Leg, 
                              addFlows :: AddFlows
                          } deriving (Eq, Show, Data, Typeable)

-------------------------------------------------------------------------- 
data Leg = FixedLeg    {
                           coupons :: [Coupon], 
                           discCurve :: String,
                           legPayerReceiver :: PayerReceiver
                       } 
         | VariableLeg {
                           coupons :: [Coupon], 
                           estCurve :: String,
                           discCurve :: String,
                           legIndex :: String, 
                           legPayerReceiver :: PayerReceiver
                       } deriving (Eq, Show, Data, Typeable)  
-------------------------------------------------------------------------- 
data Coupon = Fixed    {
                           cpStartDate :: Day, 
                           cpEndDate :: Day, 
                           cpPayDate :: Day, 
                           cpYearFrac :: Double, 
                           cpRemainingCapital :: Double, 
                           cpConvention :: Convention,
                           fxRate :: Double, 
                           fxDiscFactor :: Double, 
                           cpDiscCurve :: String
                       } 
            | Variable {
                           cpStartDate :: Day, 
                           cpEndDate :: Day, 
                           cpPayDate :: Day, 
                           cpYearFrac :: Double, 
                           cpRemainingCapital :: Double, 
                           cpConvention :: Convention,
                           varPayOff :: PayOff, 
                           varModel :: Model, 
                           varNum0 :: Double,
                           cpEstCurve :: String,
                           cpDiscCurve :: String, 
                           cpIndex :: String
                       } deriving (Eq, Show, Data, Typeable)  
-------------------------------------------------------------------------- 
data AddFlow = AddFlow {
                           afPayDate :: Day, 
                           afAmount :: Double, 
                           afDiscFactor :: Double, 
                           afPayerReceiver :: PayerReceiver
                       } deriving (Eq, Show, Data, Typeable)  
-------------------------------------------------------------------------- 
newtype AddFlows = AddFlows {
                                listAddFlows :: [AddFlow]
                            } deriving (Eq, Show, Data, Typeable)
  
-------------------------------------------------------------------------- 
-- Value Storage
data ValueStorage = ValueStorage {
                                     value :: Double, 
                                     greeks :: [Double], 
                                     subValues :: [ValueStorage]
                                 } deriving (Eq, Show, Data, Typeable)

-------------------------------------------------------------------------- 

newtype Portfolio = Portfolio { pfProducts :: [Product]}




