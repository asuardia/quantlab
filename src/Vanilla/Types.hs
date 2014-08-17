{-# LANGUAGE DeriveDataTypeable #-}
module Vanilla.Types   
    ( 
     Ready2Val (..),        Input (..),        Deal (..),             DealInfo (..), 
     Flex (..),             Product (..),      Generator (..),        Leg (..),          
     Coupon (..),           AddFlows (..),     AddFlow (..),          AnalyticalValuable, 
     ProductReady2Val (..), LegReady2Val (..), AddFlowReady2Val (..), CouponReady2Val (..),
     valuateA,              MarketZippeable,   mktZip,
     module Data.Time.Calendar,          module Utils.MyDates,     module Vanilla.ModelParameters, module Market.Indexes,
     module Utils.MyJSON,                module Vanilla.Formulas,  module Vanilla.Models,          module Vanilla.PayOffs, 
     module Market.FinantialConventions, module Market.Currencies, module Market.MarketData 
     
    ) where 

import Data.Time.Calendar
import Vanilla.ModelParameters
import Utils.MyJSON
import Utils.MyDates
import Vanilla.FormulaDispatcher
import Vanilla.Formulas
import Vanilla.Models
import Vanilla.PayOffs
import Vanilla.MarketZip
import Market.FinantialConventions
import Market.Currencies
import Market.Indexes
import Market.MarketData
------------------------------------- BASIC TYPES ---------------------------------------------
-- input
data Input = Input {
                       inputDeal :: Deal, 
                       inputMarket :: MarketData, 
                       inputModelParams :: ModelParameters
                   } deriving (Eq, Show, Data, Typeable)  
--------------------------------------------------------------------------------------
-- Deal
data Deal = Deal {
                     dealInfo :: DealInfo, 
                     dealProduct :: Product
                 } deriving (Eq, Show, Data, Typeable)  
--------------------------------------------------------------------------------------
-- Deal information
data DealInfo = DealInfo {
                             portfolio :: String, 
                             evalDate :: Day, 
                             isPricingMode :: Bool, 
                             flex :: Flex
                         } deriving (Eq, Show, Data, Typeable)  
--------------------------------------------------------------------------------------
-- Flex info
data Flex = Flex {
                     flexType :: String, 
                     blockLabel :: String
                 } deriving (Eq, Show, Data, Typeable)  
--------------------------------------------------------------------------------------

-- Products             
data Product = Swap       {
                              generator :: Generator, 
                              swLeg1 :: Leg, 
                              swLeg2 :: Leg, 
                              addFlows :: AddFlows
                          }
             | CancelSwap {
                              cacelSwap :: Product, 
                              exerciseDates :: [Day]
                          } 
             | Option     {
                              generator :: Generator, 
                              opLeg :: Leg, 
                              addFlows :: AddFlows
                          } deriving (Eq, Show, Data, Typeable)
-- Generators
data Generator = SwapGenerator 
               | OptionGenerator deriving (Eq, Show, Data, Typeable)  
--------------------------------------------------------------------------------------
data Leg = FixedLeg    {
                           coupons :: [Coupon], 
                           discCurve :: String,
                           legPayerReceiver :: PayerReceiver
                       } 
         | VariableLeg {
                           coupons :: [Coupon], 
                           discCurve :: String,
                           legIndex :: String, 
                           legPayerReceiver :: PayerReceiver
                       } deriving (Eq, Show, Data, Typeable)  
--------------------------------------------------------------------------------------
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
                           cpDiscCurve :: String, 
                           cpIndex :: String
                       } deriving (Eq, Show, Data, Typeable)  
--------------------------------------------------------------------------------------
data AddFlow = AddFlow {
                           afPayDate :: Day, 
                           afAmount :: Double, 
                           afDiscFactor :: Double, 
                           afPayerReceiver :: PayerReceiver
                       } deriving (Eq, Show, Data, Typeable)  
--------------------------------------------------------------------------------------
newtype AddFlows = AddFlows {
                                listAddFlows :: [AddFlow]
                            } deriving (Eq, Show, Data, Typeable)
  
--------------------------------------------------------------------------------------
-- Value Storage
data ValueStorage = ValueStorage {
                                     value :: Double, 
                                     greeks :: [Double], 
                                     subValues :: [ValueStorage]
                                 } deriving (Eq, Show, Data, Typeable)

----------------- TYPE VARIABLE Ready2Val AND WRAPPERS ----------------------------------------

data Ready2Val a = Ready2Val {rvDeal :: a}

newtype ProductReady2Val  = ProductReady2Val {getR2ValProduct :: Ready2Val Product}
newtype LegReady2Val      = LegReady2Val {getR2ValLeg :: Ready2Val Leg}
newtype AddFlowsReady2Val = AddFlowsReady2Val {getR2ValAddFlows :: Ready2Val AddFlows}
newtype AddFlowReady2Val  = AddFlowReady2Val {getR2ValAddFlow :: Ready2Val AddFlow}
newtype CouponReady2Val   = CouponReady2Val {getR2ValCoupon :: Ready2Val Coupon}

--------------- CLASSES FOR INTERPOLATION AND VALUATION ---------------------------------------

-- MarketZippeable class
class MarketZippeable m where
    mktZip :: ModelParameters -> MarketData -> m -> Result (Ready2Val m)  
--------------------------------------------------------------------------------------
-- AnalyticalValuable class
class AnalyticalValuable a where
    valuateA :: Bool -> a -> Result ValueStorage
---------------------------- INSTANCES OF THE CLASSES -----------------------------------------    

-- Instances of MarketZippeable

instance MarketZippeable Product where
    mktZip modParams mktData (Swap g l1 l2 af)  = do
        zpL1 <- mktZip modParams mktData l1
        zpL2 <- mktZip modParams mktData l2
        zpAf <- mktZip modParams mktData af
        return (Ready2Val (Swap g (rvDeal $ zpL1) (rvDeal $ zpL2) (rvDeal $ zpAf)))
    mktZip modParams mktData (CancelSwap sw ed) = do
        zpSw <- mktZip modParams mktData sw
        return (Ready2Val (CancelSwap (rvDeal $ zpSw) ed))
    mktZip modParams mktData (Option g l af)    = do
        zpL  <- mktZip modParams mktData l
        zpAf <- mktZip modParams mktData af
        return (Ready2Val (Option g (rvDeal $ zpL) ((rvDeal $ zpAf))))  
--------------------------------------------------------------------------------------
instance MarketZippeable Leg where
    mktZip modParams mktData (FixedLeg cs dc pr)       = do
        zpCs <- checkAllOk $ fmap (mktZip modParams mktData) cs
        return (Ready2Val (FixedLeg (fmap rvDeal zpCs) dc pr))
    mktZip modParams mktData (VariableLeg cs dc ec pr) = do
        zpCs <- checkAllOk $ fmap (mktZip modParams mktData) cs 
        return (Ready2Val (VariableLeg (fmap rvDeal zpCs) dc ec pr))  
--------------------------------------------------------------------------------------
instance MarketZippeable AddFlows where
    mktZip modParams mktData (AddFlows afs) = do
        zpAfs <- checkAllOk $ fmap (mktZip modParams mktData) afs 
        return (Ready2Val (AddFlows (fmap rvDeal zpAfs)))  
--------------------------------------------------------------------------------------
instance MarketZippeable AddFlow where
    mktZip modParams mktData (AddFlow pd a df payRec) = Ok (Ready2Val (AddFlow pd a df payRec))  
--------------------------------------------------------------------------------------
instance MarketZippeable Coupon where
    mktZip modParams mktData (Fixed sd ed pd yf rc cn rt df dc)                   = do
        (yfNew, dfNew) <- getCouponFMktModInfo modParams mktData dc sd ed pd cn
        return (Ready2Val (Fixed sd ed pd yfNew rc cn rt dfNew dc))
    mktZip modParams mktData (Variable sd ed pd yf rc cn payOff model num0 dc ci) = do 
        (yfNew, num0New, modelNew) <- getCouponVMktModInfo modParams mktData dc ci 
                                                           sd ed pd cn payOff model
        return (Ready2Val (Variable sd ed pd yfNew rc cn payOff modelNew num0New dc ci))  
--------------------------------------------------------------------------------------
-- Instances of AnalyticalValuable

instance AnalyticalValuable ProductReady2Val where
    valuateA isPricing (ProductReady2Val (Ready2Val (Swap g l1 l2 af))) = do
        valueStorageLeg1     <- valuateA isPricing (LegReady2Val (Ready2Val l1))
        valueStorageLeg2     <- valuateA isPricing (LegReady2Val (Ready2Val l2))
        valueStorageAddFlows <- valuateA isPricing (AddFlowsReady2Val (Ready2Val af))        
        return (ValueStorage (value valueStorageLeg1 + value valueStorageLeg2 + value valueStorageAddFlows)
                             [1.0, 1.0, 1.0]
                             [valueStorageLeg1, valueStorageLeg2, valueStorageAddFlows])
    valuateA isPricing (ProductReady2Val (Ready2Val (Option g l af)))   = do
        valueStorageLeg      <- valuateA isPricing (LegReady2Val (Ready2Val l))
        valueStorageAddFlows <- valuateA isPricing (AddFlowsReady2Val (Ready2Val af))  
        return (ValueStorage (value valueStorageLeg + value valueStorageAddFlows)
                             [1.0, 1.0, 1.0]
                             [valueStorageLeg, valueStorageAddFlows])
    valuateA isPricing (ProductReady2Val (Ready2Val x))                 = 
        Error "This is not an analytical valuable product."  
--------------------------------------------------------------------------------------
instance AnalyticalValuable LegReady2Val where
    valuateA isPricing (LegReady2Val (Ready2Val (l))) = do
        listCoupons <- checkAllOk $ fmap (valuateA isPricing) 
                                         (fmap CouponReady2Val (fmap Ready2Val (coupons l)))
        return (ValueStorage (payrec * (sum $ fmap value listCoupons)) [1,1,1,1,1] listCoupons)
        where payrec = case (legPayerReceiver l) of PAYER    -> (-1.0)  
                                                    RECEIVER -> 1.0  
--------------------------------------------------------------------------------------
instance AnalyticalValuable AddFlowsReady2Val where
    valuateA isPricing (AddFlowsReady2Val (Ready2Val (afs))) = do
        listAF <- checkAllOk $ fmap (valuateA isPricing) 
                                    (fmap AddFlowReady2Val (fmap Ready2Val (listAddFlows afs)))
        return (ValueStorage (sum $ fmap value listAF) [1,1,1,1,1] listAF)  
--------------------------------------------------------------------------------------
instance AnalyticalValuable AddFlowReady2Val where
    valuateA isPricing (AddFlowReady2Val (Ready2Val (AddFlow pd a df PAYER)))    = 
        Ok (ValueStorage (negate a * df) [1.0, 1.0, 1.0] [])
    valuateA isPricing (AddFlowReady2Val (Ready2Val (AddFlow pd a df RECEIVER))) = 
        Ok (ValueStorage (a * df) [1.0, 1.0, 1.0] [])  
--------------------------------------------------------------------------------------
instance AnalyticalValuable CouponReady2Val where
    valuateA isPricing (CouponReady2Val (Ready2Val (Fixed sd ed pd yf rc (LIN, fc) rt df dc)))            = 
        Ok (ValueStorage (yf * rc * rt * df) [1.0, 1.0, 1.0] [])
    valuateA isPricing (CouponReady2Val (Ready2Val (Fixed sd ed pd yf rc (YIELD, fc) rt df dc)))          = 
        Ok (ValueStorage (((1 + rt) ** yf - 1) * rc * df) [1.0, 1.0, 1.0] [])
    valuateA isPricing (CouponReady2Val (Ready2Val (Variable sd ed pd yf rc cn payOff model num0 dc ci))) = 
        Ok (ValueStorage (yf * rc * (expectation payOff model) * num0) [1.0, 1.0, 1.0] [])
  







