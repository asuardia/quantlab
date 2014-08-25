module Vanilla.FormulaDispatcher   
    ( 
     expectation,
     calcGreeks
    ) where 
    
import Data.Time.Calendar
import Vanilla.PayOffs
import Vanilla.Models
import Vanilla.Formulas

-- Expectations of PayOffs with a model                              
----------------------------------------------------------------------------------------------------
                       
expectation :: PayOff ->                     Model ->                              Double
expectation   (Libor fx st nd py cn mr)      (Forward rD f)                      = f + mr
expectation   (Libor fx st nd py cn mr)      (ForwardNonStandard rD e t2P f v)   = (hullAdjustmentFormula v f e t2P) + mr
expectation   (Caplet fx st nd py cn mr k)   (Black rD e v f)                    = blackFormulaCall f (k - mr) e v
expectation   (Floorlet fx st nd py cn mr k) (Black rD e v f)                    = blackFormulaPut f (k - mr) e v
expectation   (Caplet fx st nd py cn mr k)   (BlackNonStandard rD e t2P v f vAd) = let fAd = hullAdjustmentFormula v f e t2P 
                                                                                   in blackFormulaCall fAd (k - mr) e v
expectation   (Floorlet fx st nd py cn mr k) (BlackNonStandard rD e t2P v f vAd) = let fAd = hullAdjustmentFormula v f e t2P
                                                                                   in blackFormulaPut fAd (k - mr) e v
expectation   (CMS fx ds mt cn mr)      
              (HaganRepSABRRBS2 rD e f v b r vv xp xM n m k)                     = f + mr
expectation   x                              y                                   = 0.0
----------------------------------------------------------------------------------------------------

calcGreeks :: PayOff ->                     Model ->                              [Double]
calcGreeks   (Libor fx st nd py cn mr)      (Forward rD f)                      = [1.0]
calcGreeks   (Libor fx st nd py cn mr)      (ForwardNonStandard rD e t2P f v)   = [1.0, 1.0]
calcGreeks   (Caplet fx st nd py cn mr k)   (Black rD e v f)                    = [1.0, 1.0]
calcGreeks   (Floorlet fx st nd py cn mr k) (Black rD e v f)                    = [1.0, 1.0]
calcGreeks   (Caplet fx st nd py cn mr k)   (BlackNonStandard rD e t2P v f vAd) = [1.0, 1.0, 1.0]
calcGreeks   (Floorlet fx st nd py cn mr k) (BlackNonStandard rD e t2P v f vAd) = [1.0, 1.0, 1.0]
calcGreeks   (CMS fx ds mt cn mr)      
              (HaganRepSABRRBS2 rD e f v b r vv xp xM n m k)                    = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
calcGreeks   x                              y                                   = [0.0]
----------------------------------------------------------------------------------------------------
