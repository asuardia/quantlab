{-# OPTIONS_GHC -XFlexibleContexts #-}

import qualified Data.Monoid as Mo  
import System.IO  
import System.Environment
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types
import Interfaces.ILInterface
import Vanilla.Types
import Vanilla.Instances
import Vanilla.Containers
import Vanilla.ModelParameters
import Market.ProdExamples
import Market.Generators
import Market.MarketData

main = do

    marketJSON         <- readFile "../inputs/market.in"
    modParamsJSON      <- readFile "../inputs/modelParams.in"
    let marketData      = decodeJSON marketJSON    :: MarketData 
    let modelParameters = decodeJSON modParamsJSON :: ModelParameters       
    let evDate          = refDate $ (curves marketData)!!0
        viewer              = do
            product1       <- builder (template1 evDate)
            product2       <- builder (template2 evDate)
            product3       <- builder (template3 evDate)
            product4       <- builder (template4 evDate)
            let portfolio1  = Portfolio [product1, product2, product3]    
            let portfolio2  = Portfolio [product4]    
            let portfolio3  = Mo.mappend portfolio1 portfolio2
            output         <- process modelParameters marketData product4
            return output
        
    writeFile "../logs/output.json" (showResult viewer)
    writeFile "../logs/output.json" (showResult viewer)
    return ()
          
--------------------------------------------------------------------------------------
 
process :: (MarketZippeable p, AnalyticalValuable (Ready2Val p), 
           (Mappeable (Ready2Val p))) => 
           ModelParameters -> MarketData -> p  -> Result Viewer
process    modelParameters    marketData    pr  = do
    let isPricing   = True
    pReadyToVal    <- mktZip modelParameters marketData pr
    valProduct     <- valuateA isPricing pReadyToVal
    viewerSAGreeks <- mapG valProduct pReadyToVal
    viewerVal      <- mapV valProduct pReadyToVal
    return (Viewer viewerVal viewerSAGreeks)
    
--------------------------------------------------------------------------------------
showResult :: (Data a) => 
              Result a -> String    
showResult    (Ok v)    = encodeJSON v
showResult    (Error v) = v       
--------------------------------------------------------------------------------------
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        