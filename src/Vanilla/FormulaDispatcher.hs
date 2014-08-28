module Vanilla.FormulaDispatcher   
    ( 
     expectation, calcGreeks, expectationSwptn, calcGreeksSwptn
    ) where 
    
import Data.Time.Calendar
import Vanilla.PayOffs
import Vanilla.Models
import Vanilla.Formulas
import Market.FinantialConventions
import Utils.MyUtils
              
-------------------------------------------------------------------------- 
-- Expectations of PayOffs with a model     
                       
expectation :: PayOff -> Model -> Double
expectation   (Libor fx st nd py cn mr)      
              (Forward rD f)                                 
              = (fst4 f) + mr
-------------------------------------------------------------------------- 
expectation   (Libor fx st nd py cn mr)      
              (ForwardNonStandard rD e t2P f v)              
              = (hullAdjustmentFormula v (fst4 f) e t2P) + mr
-------------------------------------------------------------------------- 
expectation   (Caplet fx st nd py cn mr k)   
              (Black rD e v f)                               
              = blackFormulaCall (fst4 f) (k - mr) e v
-------------------------------------------------------------------------- 
expectation   (Floorlet fx st nd py cn mr k) 
              (Black rD e v f)                               
              = blackFormulaPut (fst4 f) (k - mr) e v
-------------------------------------------------------------------------- 
expectation   (Caplet fx st nd py cn mr k)   
              (BlackNonStandard rD e t2P v f vAd)            
              = let fAd = hullAdjustmentFormula v (fst4 f) e t2P 
                in blackFormulaCall fAd (k - mr) e v
-------------------------------------------------------------------------- 
expectation   (Floorlet fx st nd py cn mr k) 
              (BlackNonStandard rD e t2P v f vAd)            
              = let fAd = hullAdjustmentFormula v (fst4 f) e t2P
                in blackFormulaPut fAd (k - mr) e v
-------------------------------------------------------------------------- 
expectation   (CMS fx ds mt cn mr)      
              (HaganRepSABRRBS2 rD e f v b r vv xp xM n m k) 
              = (fst4 f) + mr
-------------------------------------------------------------------------- 
expectation   x                              y               = 0.0
-------------------------------------------------------------------------- 
-------------------------------------------------------------------------- 

calcGreeks :: PayOff -> Model -> [Double]
calcGreeks   (Libor fx st nd py cn mr)      
             (Forward rD f) = (calcGreeksFdFw f)
-------------------------------------------------------------------------- 
calcGreeks   (Libor fx st nd py cn mr)      
             (ForwardNonStandard rD e t2P f v) = (calcGreeksFdFw f) ++ [1.0]
-------------------------------------------------------------------------- 
calcGreeks   (Caplet fx st nd py cn mr k)   
             (Black rD e v f) = (calcGreeksFdFw f) ++ [1.0]
-------------------------------------------------------------------------- 
calcGreeks   (Floorlet fx st nd py cn mr k) 
             (Black rD e v f) = (calcGreeksFdFw f) ++ [1.0]
-------------------------------------------------------------------------- 
calcGreeks   (Caplet fx st nd py cn mr k)   
             (BlackNonStandard rD e t2P v f vAd) = (calcGreeksFdFw f) ++ [1.0, 1.0]
-------------------------------------------------------------------------- 
calcGreeks   (Floorlet fx st nd py cn mr k) 
             (BlackNonStandard rD e t2P v f vAd) = (calcGreeksFdFw f) ++ [1.0, 1.0]
-------------------------------------------------------------------------- 
calcGreeks   (CMS fx ds mt cn mr)      
             (HaganRepSABRRBS2 rD e f v b r vv xp xM n m k)                    
             = (calcGreeksFdFw f) 
             ++ [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
-------------------------------------------------------------------------- 
calcGreeks   x y                                 = [0.0]
-------------------------------------------------------------------------- 
calcGreeksFdFw :: (Double, [Day], [Double], [Double]) -> [Double]
calcGreeksFdFw    fw = fstElem ++ middleElems ++ lstElem
    where
          fstElem = [1 / ann]
          middleElems = fmap ((negate ((head fds) - (last fds))/(ann * ann)) *) (init dts)
          lstElem = [((-ann) - (last dts) * ((head fds) - (last fds)))/(ann * ann)]
          fds = trd4 fw
          dts = fth4 fw
          ann = sum $ zipWith (*) (tail fds) dts
          
          
-------------------------------------------------------------------------- 

expectationSwptn :: SwaptionPayOff -> PayerReceiver -> Double -> Model 
                 -> Double
-------------------------------------------------------------------------- 
expectationSwptn    Delivery          PAYER            str
                    (Black rD e v f)
                  = blackFormulaCall (fst4 f) str e v
--------------------------------------------------------------------------  
expectationSwptn    Delivery          RECEIVER         str
                    (Black rD e v f)
                  = blackFormulaPut (fst4 f) str e v
-------------------------------------------------------------------------- 
expectationSwptn   a b c d = 0.0
-------------------------------------------------------------------------- 


calcGreeksSwptn :: SwaptionPayOff -> PayerReceiver -> Double -> Model 
                -> [Double]
-------------------------------------------------------------------------- 
calcGreeksSwptn    Delivery          PAYER            str
                   (Black rD e v f)
                 = [1.0, 1.0]
-------------------------------------------------------------------------- 
calcGreeksSwptn    Delivery          RECEIVER         str
                   (Black rD e v f)
                 = [1.0, 1.0]
-------------------------------------------------------------------------- 
calcGreeksSwptn    a b c d = [0.0]
--------------------------------------------------------------------------  



