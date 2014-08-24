{-# OPTIONS_GHC -XFlexibleContexts #-}

import System.IO  
import System.Environment
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types

import Interfaces.ILInterface
import Vanilla.Types
import Vanilla.Instances
import Vanilla.SAGreeks
--import Vanilla.Viewer
import Market.Generators

main = do

    marketJSON         <- readFile "../inputs/market.in"
    modParamsJSON      <- readFile "../inputs/modelParams.in"
    let marketData      = decodeJSON marketJSON    :: MarketData 
    let modelParameters = decodeJSON modParamsJSON :: ModelParameters       
    let evDate          = refDate $ (curves marketData)!!0
    let product1        = (\(Ok x) -> x) $ builder (template1 evDate)
    let product2        = (\(Ok x) -> x) $ builder (template1 evDate)
    let product3        = (\(Ok x) -> x) $ builder (template1 evDate)
    let portfolio       = Portfolio [product1, product2, product3]    
    let jsonOutput      = showResult $ process modelParameters marketData portfolio     
    writeFile "../logs/output.json" jsonOutput      
    return ()
          
--------------------------------------------------------------------------------------
 
process :: (MarketZippeable p, AnalyticalValuable (Ready2Val p), (Mappeable (Ready2Val p))) => 
           ModelParameters -> MarketData -> p -> Result Viewer
process    modelParameters    marketData    pr  = do
    let isPricing   = True
    pReadyToVal    <- mktZip modelParameters marketData pr
    valProduct     <- valuateA isPricing pReadyToVal
    viewerSAGreeks <- mapG modelParameters marketData valProduct pReadyToVal
    viewerVal      <- mapV valProduct pReadyToVal
    return (Viewer viewerVal viewerSAGreeks)
    
--------------------------------------------------------------------------------------
showResult :: (Data a) => Result a -> String    
showResult    (Ok v)           = encodeJSON v
showResult    (Error v)        = v
    
    
--------------------------------------------------------------------------------------
template1 eD = SwapTemplate { 
                           prCapital   = 100000000,
                           prGenerator = euribor3mSwGen,
                           prStartDay  = addGregorianMonthsClip 1 eD,
                           prMat = 5,
                           legInfo1 = LegFixInfo {
                                                     lPayRec    = PAYER, 
                                                     lFixRate   = 0.02,
                                                     lModPay    = PayInArrears,
                                                     lCon       = (LIN, THIRTY360),
                                                     lDiscCurve = "EUR_CALMNY_DISC"
                                                 },
                           legInfo2 = LegFloatInfo {
                                                       lPayRec    = RECEIVER, 
                                                       lModFixing = FixUpFront,
                                                       lModPay    = PayInArrears,
                                                       lCon       = (LIN, ACT360),
                                                       lMargin    = 0.0,
                                                       lPayOff    = liborPO,
                                                       lModel     = forwardMO,
                                                       lEstCurve  = "EUR_FUTSWAP_3M",
                                                       lDiscCurve = "EUR_CALMNY_DISC"
                                                   } 
                         }    
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        