{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
module Vanilla.Instances   
    ( 
     Ready2Val (..),        AnalyticalValuable, 
     PortfolioReady2Val (..), ProductReady2Val (..), LegReady2Val (..), AddFlowReady2Val (..), CouponReady2Val (..),
     valuateA,              MarketZippeable,   mktZip
    ) where 

import Vanilla.Types
import Vanilla.FormulaDispatcher
import Vanilla.MarketZip

----------------- TYPE VARIABLE Ready2Val AND WRAPPERS ----------------------------------------

data Ready2Val a = Ready2Val {rvDeal :: a}

newtype PortfolioReady2Val = PortfolioReady2Val {getR2ValPF       :: Ready2Val Portfolio}
newtype ProductReady2Val   = ProductReady2Val   {getR2ValProduct  :: Ready2Val Product}
newtype LegReady2Val       = LegReady2Val       {getR2ValLeg      :: Ready2Val Leg}
newtype AddFlowsReady2Val  = AddFlowsReady2Val  {getR2ValAddFlows :: Ready2Val AddFlows}
newtype AddFlowReady2Val   = AddFlowReady2Val   {getR2ValAddFlow  :: Ready2Val AddFlow}
newtype CouponReady2Val    = CouponReady2Val    {getR2ValCoupon   :: Ready2Val Coupon}

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
instance MarketZippeable Portfolio where
    mktZip modParams mktData pf = do
       rvPrs     <- checkAllOk $ fmap (mktZip modParams mktData) $ pfProducts pf 
       let zipPrs = fmap rvDeal rvPrs 
       return (Ready2Val (Portfolio zipPrs))
--------------------------------------------------------------------------------------
       
instance MarketZippeable Product where
    mktZip modParams mktData (Swap l1 l2 af)  = do
        zpL1 <- mktZip modParams mktData l1
        zpL2 <- mktZip modParams mktData l2
        zpAf <- mktZip modParams mktData af
        return (Ready2Val (Swap (rvDeal $ zpL1) (rvDeal $ zpL2) (rvDeal $ zpAf)))
    mktZip modParams mktData (CancelSwap sw ed) = do
        zpSw <- mktZip modParams mktData sw
        return (Ready2Val (CancelSwap (rvDeal $ zpSw) ed))
    mktZip modParams mktData (Option l af)    = do
        zpL  <- mktZip modParams mktData l
        zpAf <- mktZip modParams mktData af
        return (Ready2Val (Option (rvDeal $ zpL) ((rvDeal $ zpAf))))  
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
    mktZip modParams mktData coupon = do
        coupon' <- getCouponMktModInfo modParams mktData coupon
        return (Ready2Val (coupon'))
--------------------------------------------------------------------------------------
-- Instances of AnalyticalValuable
    {-
instance AnalyticalValuable PortfolioReady2Val where
    valuateA isPricing (PortfolioReady2Val (Ready2Val pf)) = do
        let prs = pfProducts pf
        lsVS <- checkAllOk $ fmap ((valuateA isPricing) . ProductReady2Val . Ready2Val) prs 
        return (ValueStorage (sum $ fmap value lsVS) (take (length lsVS) [1,1 ..]) lsVS )-}

instance AnalyticalValuable (Ready2Val Portfolio) where
    valuateA isPricing (Ready2Val pf) = do
        let prs = pfProducts pf
        lsVS <- checkAllOk $ fmap ((valuateA isPricing) . Ready2Val) prs 
        return (ValueStorage (sum $ fmap value lsVS) (take (length lsVS) [1,1 ..]) lsVS )        
        
--------------------------------------------------------------------------------------
{-instance AnalyticalValuable ProductReady2Val where
    valuateA isPricing (ProductReady2Val (Ready2Val (Swap l1 l2 af))) = do
        valueStorageLeg1     <- valuateA isPricing (LegReady2Val (Ready2Val l1))
        valueStorageLeg2     <- valuateA isPricing (LegReady2Val (Ready2Val l2))
        valueStorageAddFlows <- valuateA isPricing (AddFlowsReady2Val (Ready2Val af))        
        return (ValueStorage (value valueStorageLeg1 + value valueStorageLeg2 + value valueStorageAddFlows)
                             [1.0, 1.0, 1.0]
                             [valueStorageLeg1, valueStorageLeg2, valueStorageAddFlows])
    valuateA isPricing (ProductReady2Val (Ready2Val (Option l af)))   = do
        valueStorageLeg      <- valuateA isPricing (LegReady2Val (Ready2Val l))
        valueStorageAddFlows <- valuateA isPricing (AddFlowsReady2Val (Ready2Val af))  
        return (ValueStorage (value valueStorageLeg + value valueStorageAddFlows)
                             [1.0, 1.0]
                             [valueStorageLeg, valueStorageAddFlows])
    valuateA isPricing (ProductReady2Val (Ready2Val x))                 = 
        Error "This is not an analytical valuable product."  -}
        
instance AnalyticalValuable (Ready2Val Product) where
    valuateA isPricing (Ready2Val (Swap l1 l2 af)) = do
        valueStorageLeg1     <- valuateA isPricing (Ready2Val l1)
        valueStorageLeg2     <- valuateA isPricing (Ready2Val l2)
        valueStorageAddFlows <- valuateA isPricing (Ready2Val af)        
        return (ValueStorage (value valueStorageLeg1 + value valueStorageLeg2 + value valueStorageAddFlows)
                             [1.0, 1.0, 1.0]
                             [valueStorageLeg1, valueStorageLeg2, valueStorageAddFlows])
    valuateA isPricing (Ready2Val (Option l af))   = do
        valueStorageLeg      <- valuateA isPricing (Ready2Val l)
        valueStorageAddFlows <- valuateA isPricing (Ready2Val af)  
        return (ValueStorage (value valueStorageLeg + value valueStorageAddFlows)
                             [1.0, 1.0]
                             [valueStorageLeg, valueStorageAddFlows])
    valuateA isPricing (Ready2Val x)                 = 
        Error "This is not an analytical valuable product."          
--------------------------------------------------------------------------------------
{-instance AnalyticalValuable LegReady2Val where
    valuateA isPricing (LegReady2Val (Ready2Val (l))) = do
        listCoupons <- checkAllOk $ fmap (valuateA isPricing) 
                                         (fmap CouponReady2Val (fmap Ready2Val (coupons l)))
        return (ValueStorage (payrec * (sum $ fmap value listCoupons)) (fmap (\_ -> 1.0) listCoupons) listCoupons)
        where payrec = case (legPayerReceiver l) of PAYER    -> (-1.0)  
                                                    RECEIVER -> 1.0  
 -}                                                   

instance AnalyticalValuable (Ready2Val Leg) where
    valuateA isPricing (Ready2Val (l)) = do
        listCoupons <- checkAllOk $ fmap (valuateA isPricing) (fmap Ready2Val (coupons l))
        return (ValueStorage (payrec * (sum $ fmap value listCoupons)) (fmap (\_ -> 1.0) listCoupons) listCoupons)
        where payrec = case (legPayerReceiver l) of PAYER    -> (-1.0)  
                                                    RECEIVER -> 1.0                                                      
--------------------------------------------------------------------------------------
{-instance AnalyticalValuable AddFlowsReady2Val where
    valuateA isPricing (AddFlowsReady2Val (Ready2Val (afs))) = do
        listAF <- checkAllOk $ fmap (valuateA isPricing) 
                                    (fmap AddFlowReady2Val (fmap Ready2Val (listAddFlows afs)))
        return (ValueStorage (sum $ fmap value listAF) (fmap (\_ -> 1.0) listAF) listAF)  
-}        
        
instance AnalyticalValuable (Ready2Val AddFlows) where
    valuateA isPricing (Ready2Val (afs)) = do
        listAF <- checkAllOk $ fmap (valuateA isPricing) (fmap Ready2Val (listAddFlows afs))
        return (ValueStorage (sum $ fmap value listAF) (fmap (\_ -> 1.0) listAF) listAF)  
--------------------------------------------------------------------------------------
{-instance AnalyticalValuable AddFlowReady2Val where
    valuateA isPricing (AddFlowReady2Val (Ready2Val (AddFlow pd a df PAYER)))    = 
        Ok (ValueStorage (negate a * df) [negate a] [])
    valuateA isPricing (AddFlowReady2Val (Ready2Val (AddFlow pd a df RECEIVER))) = 
        Ok (ValueStorage (a * df) [a] [])  
 -}       
        
instance AnalyticalValuable (Ready2Val AddFlow) where
    valuateA isPricing (Ready2Val (AddFlow pd a df PAYER))    = 
        Ok (ValueStorage (negate a * df) [negate a] [])
    valuateA isPricing (Ready2Val (AddFlow pd a df RECEIVER)) = 
        Ok (ValueStorage (a * df) [a] [])  
--------------------------------------------------------------------------------------
{-instance AnalyticalValuable CouponReady2Val where
    valuateA isPricing (CouponReady2Val (Ready2Val (Fixed sd ed pd yf rc (LIN, fc) rt df dc)))            = 
        Ok (ValueStorage (yf * rc * rt * df) [yf * rc * rt] [])
    valuateA isPricing (CouponReady2Val (Ready2Val (Fixed sd ed pd yf rc (YIELD, fc) rt df dc)))          = 
        Ok (ValueStorage (((1 + rt) ** yf - 1) * rc * df) [((1 + rt) ** yf - 1) * rc] [])
    valuateA isPricing (CouponReady2Val (Ready2Val (Variable sd ed pd yf rc cn payOff model num0 dc ci))) = 
        Ok (ValueStorage (noRandom * expect) (greeksDesc ++ greeksSA) [])
            where 
                  noRandom   = yf * rc * num0
                  expect     = expectation payOff model
                  greeksSA   = fmap (noRandom *) (calcGreeks payOff model)
                  greeksDesc = [yf * rc * expect]
-}  
instance AnalyticalValuable (Ready2Val Coupon) where
    valuateA isPricing (Ready2Val (Fixed sd ed pd yf rc (LIN, fc) rt df dc))            = 
        Ok (ValueStorage (yf * rc * rt * df) [yf * rc * rt] [])
    valuateA isPricing (Ready2Val (Fixed sd ed pd yf rc (YIELD, fc) rt df dc))          = 
        Ok (ValueStorage (((1 + rt) ** yf - 1) * rc * df) [((1 + rt) ** yf - 1) * rc] [])
    valuateA isPricing (Ready2Val (Variable sd ed pd yf rc cn payOff model num0 dc ci)) = 
        Ok (ValueStorage (noRandom * expect) (greeksDesc ++ greeksSA) [])
            where 
                  noRandom   = yf * rc * num0
                  expect     = expectation payOff model
                  greeksSA   = fmap (noRandom *) (calcGreeks payOff model)
                  greeksDesc = [yf * rc * expect]







