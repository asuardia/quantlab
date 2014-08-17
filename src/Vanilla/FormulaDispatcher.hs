module Vanilla.FormulaDispatcher   
    ( 
     expectation
    ) where 
    
import Data.Time.Calendar
import Vanilla.PayOffs
import Vanilla.Models
import Vanilla.Formulas

-- Expectations of PayOffs with a model                                                     
expectation :: PayOff ->                     Model ->                     Double
expectation   (Libor fx st nd py cn mr)      (Forward f)                = f + mr
expectation   (Libor fx st nd py cn mr)      (ForwardNonStandard f v)   = (hullAdjustmentFormula v f fx py) + mr
expectation   (Caplet fx st nd py cn mr k)   (Black v f)                = blackFormula f (k - mr) fx v 'c'
expectation   (Floorlet fx st nd py cn mr k) (Black v f)                = blackFormula f (k - mr) fx v 'f'
expectation   (Caplet fx st nd py cn mr k)   (BlackNonStandard v f vAd) = let fAd = hullAdjustmentFormula vAd f fx py 
                                                                          in blackFormula fAd (k - mr) fx v 'c'
expectation   (Floorlet fx st nd py cn mr k) (BlackNonStandard v f vAd) = let fAd = hullAdjustmentFormula vAd f fx py 
                                                                          in blackFormula fAd (k - mr) fx v 'f'
expectation   x                              y                          = 0.0
