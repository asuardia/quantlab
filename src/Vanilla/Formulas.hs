module Vanilla.Formulas   
    ( 
     blackFormulaCall, blackFormulaPut, hullAdjustmentFormula
    ) where
    
import Data.Time.Calendar
import Vanilla.BlackScholes 

-------------------------------------------------------------------------- 
-- Formulas
blackFormulaCall :: Double -> Double -> Double -> Double -> Double
blackFormulaCall    forward   strike    expiry    vol     
                 = callValue forward strike 0.0 expiry vol 0.0
-------------------------------------------------------------------------- 

blackFormulaPut :: Double -> Double -> Double -> Double -> Double
blackFormulaPut    forward   strike    expiry    vol     
                = putValue forward strike 0.0 expiry vol 0.0
-------------------------------------------------------------------------- 

hullAdjustmentFormula :: Double -> Double -> Double -> Double -> Double
hullAdjustmentFormula    vol       forward   fixing    payment = forward