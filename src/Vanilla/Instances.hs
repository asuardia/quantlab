{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
module Vanilla.Instances   
    ( 
     Ready2Val (..), AnalyticalValuable, 
     valuateA,       MarketZippeable,   mktZip
    ) where 

import qualified Data.Monoid as Mo  
import Market.MarketData
import Market.FinantialConventions
import Utils.MyJSON
import Utils.MyUtils
import Vanilla.Types
import Vanilla.ModelParameters
import Vanilla.FormulaDispatcher
import Vanilla.MarketZip

----------------- TYPE VARIABLE Ready2Val AND WRAPPERS -------------------

data Ready2Val a = Ready2Val {rvDeal :: a}

--------------- CLASSES FOR INTERPOLATION AND VALUATION ------------------

-- MarketZippeable class
class MarketZippeable m where
    mktZip :: ModelParameters -> MarketData -> m -> Result (Ready2Val m)  
-------------------------------------------------------------------------- 
-- AnalyticalValuable class
class AnalyticalValuable a where
    valuateA :: Bool -> a -> Result ValueStorage
---------------------------- INSTANCES OF THE CLASSES --------------------    

instance Mo.Monoid Portfolio where  
    mempty = Portfolio []
    mappend (Portfolio prs1) (Portfolio prs2) = Portfolio (prs1 ++ prs2)
-------------------------------------------------------------------------- 
    
-- Instances of MarketZippeable
instance MarketZippeable Portfolio where
    mktZip modParams mktData pf = do
       rvPrs     <- checkAllOk $ fmap (mktZip modParams mktData) $ pfProducts pf 
       let zipPrs = fmap rvDeal rvPrs 
       return (Ready2Val (Portfolio zipPrs))
-------------------------------------------------------------------------- 
       
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
    mktZip modParams mktData (Swaption s ed po str n0 mo c rc)    = do
        zpSwap <- mktZip modParams mktData s
        zpSwpn <- getSwptnMktModInfo modParams mktData 
                      (Swaption (rvDeal zpSwap) ed po str n0 mo c rc)
        return (Ready2Val zpSwpn)
-------------------------------------------------------------------------- 
instance MarketZippeable Leg where
    mktZip modParams mktData (FixedLeg cs dc pr)       = do
        zpCs <- checkAllOk $ fmap (mktZip modParams mktData) cs
        return (Ready2Val (FixedLeg (fmap rvDeal zpCs) dc pr))
    mktZip modParams mktData (VariableLeg cs ec dc i pr) = do
        zpCs <- checkAllOk $ fmap (mktZip modParams mktData) cs 
        return (Ready2Val (VariableLeg (fmap rvDeal zpCs) ec dc i pr))  
-------------------------------------------------------------------------- 
instance MarketZippeable AddFlows where
    mktZip modParams mktData (AddFlows afs) = do
        zpAfs <- checkAllOk $ fmap (mktZip modParams mktData) afs 
        return (Ready2Val (AddFlows (fmap rvDeal zpAfs)))  
-------------------------------------------------------------------------- 
instance MarketZippeable AddFlow where
    mktZip modParams mktData (AddFlow pd a df payRec) 
        = Ok (Ready2Val (AddFlow pd a df payRec))  
-------------------------------------------------------------------------- 
instance MarketZippeable Coupon where
    mktZip modParams mktData coupon = do
        coupon' <- getCouponMktModInfo modParams mktData coupon
        return (Ready2Val (coupon'))
-------------------------------------------------------------------------- 
instance AnalyticalValuable (Ready2Val Portfolio) where
    valuateA isPricing (Ready2Val pf) = do
        let prs = pfProducts pf
        lsVS <- checkAllOk $ fmap ((valuateA isPricing) . Ready2Val) prs 
        return (ValueStorage (sum $ fmap value lsVS) (take (length lsVS) [1,1 ..]) lsVS )        
        
-------------------------------------------------------------------------- 
        
instance AnalyticalValuable (Ready2Val Product) where
    valuateA isPricing (Ready2Val (Swap l1 l2 af)) = do
        valueStorageLeg1     <- valuateA isPricing (Ready2Val l1)
        valueStorageLeg2     <- valuateA isPricing (Ready2Val l2)
        valueStorageAddFlows <- valuateA isPricing (Ready2Val af)        
        return (ValueStorage (value valueStorageLeg1 + value valueStorageLeg2 
                                + value valueStorageAddFlows)
                             [1.0, 1.0, 1.0]
                             [valueStorageLeg1, valueStorageLeg2, valueStorageAddFlows])
    valuateA isPricing (Ready2Val (Option l af))   = do
        valueStorageLeg      <- valuateA isPricing (Ready2Val l)
        valueStorageAddFlows <- valuateA isPricing (Ready2Val af)  
        return (ValueStorage (value valueStorageLeg + value valueStorageAddFlows)
                             [1.0, 1.0]
                             [valueStorageLeg, valueStorageAddFlows])
    valuateA isPricing (Ready2Val sw@(Swaption {})) 
        = Ok (ValueStorage value (greeksDesc ++ greeksSA) [])
        where 
              expect     = expectationSwptn (swptnTypePO sw) 
                                            (legPayerReceiver $ swLeg1 (swptnSwap sw))
                                            (swptnStrike sw) 
                                            (swptnModel sw)
              greeks     = calcGreeksSwptn  (swptnTypePO sw) 
                                            (legPayerReceiver $ swLeg1 (swptnSwap sw))
                                            (swptnStrike sw) 
                                            (swptnModel sw)
              noRandom   = swptnCap sw 
              value      = (fst4 $ swptnNum0 sw) * noRandom * expect
              greeksSA   = fmap (((fst4 $ swptnNum0 sw) * noRandom) *) greeks
              greeksDesc = fmap ((noRandom * expect) *) (fth4 $ swptnNum0 sw)
                                          
    valuateA isPricing (Ready2Val x)                 = 
        Error "This is not an analytical valuable product."          
-------------------------------------------------------------------------- 

instance AnalyticalValuable (Ready2Val Leg) where
    valuateA isPricing (Ready2Val (l)) = do
        listCoupons <- checkAllOk $ fmap (valuateA isPricing) 
                                         (fmap Ready2Val (coupons l))
        return (ValueStorage (payrec * (sum $ fmap value listCoupons)) 
                             (fmap (\_ -> 1.0) listCoupons) 
                              listCoupons)
        where payrec = case (legPayerReceiver l) of PAYER    -> (-1.0)  
                                                    RECEIVER -> 1.0              
--------------------------------------------------------------------------    
    
instance AnalyticalValuable (Ready2Val AddFlows) where
    valuateA isPricing (Ready2Val (afs)) = do
        listAF <- checkAllOk $ fmap (valuateA isPricing) 
                                    (fmap Ready2Val (listAddFlows afs))
        return (ValueStorage (sum $ fmap value listAF) 
                             (fmap (\_ -> 1.0) listAF) 
                              listAF) 
-------------------------------------------------------------------------- 
      
instance AnalyticalValuable (Ready2Val AddFlow) where
    valuateA isPricing (Ready2Val (AddFlow pd a df PAYER))    = 
        Ok (ValueStorage (negate a * df) [negate a] [])
    valuateA isPricing (Ready2Val (AddFlow pd a df RECEIVER)) = 
        Ok (ValueStorage (a * df) [a] [])  
-------------------------------------------------------------------------- 

instance AnalyticalValuable (Ready2Val Coupon) where
    valuateA isPricing (Ready2Val (Fixed sd ed pd yf rc (LIN, fc) rt df dc))               = 
        Ok (ValueStorage (yf * rc * rt * df) [yf * rc * rt] [])
    valuateA isPricing (Ready2Val (Fixed sd ed pd yf rc (YIELD, fc) rt df dc))             = 
        Ok (ValueStorage (((1 + rt) ** yf - 1) * rc * df) [((1 + rt) ** yf - 1) * rc] [])
    valuateA isPricing (Ready2Val (Variable sd ed pd yf rc cn payOff model num0 ec dc ci)) = 
        Ok (ValueStorage ((fst4 num0) * noRandom * expect) (greeksDesc ++ greeksSA) [])
            where 
                  noRandom   = yf * rc 
                  expect     = expectation payOff model
                  greeksSA   = fmap (((fst4 num0) * noRandom) *) (calcGreeks payOff model)
                  greeksDesc = fmap ((noRandom * expect) *) (fth4 num0)







