module Vanilla.Formulas   
    ( 
     blackFormula, 
     hullAdjustmentFormula
    ) where
    
import Data.Time.Calendar

-- Formulas
blackFormula :: Double -> Double -> Day -> Double -> Char ->   Double
blackFormula    forward   strike    expiry vol       optType = 1.0

hullAdjustmentFormula :: Double -> Double -> Day -> Day ->    Double
hullAdjustmentFormula    vol       forward   fixing payment = 1.0