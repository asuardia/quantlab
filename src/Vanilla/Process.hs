{-# LANGUAGE DeriveDataTypeable #-}
module Vanilla.Process
    ( 
     process 
    ) where 

import Vanilla.Types
import Vanilla.Instances
import Vanilla.SAGreeks

-- Main function where we process the inputs: get mkt and product info,
-- zip them together and then valuateA the product

process :: String -> Result String    
process jsonInput = do  
    let input           = decodeJSON jsonInput :: Input
    let deal            = inputDeal input
    let product         = dealProduct deal
    let marketData      = inputMarket input
    let modelParameters = inputModelParams input
    let isPricing       = isPricingMode $ dealInfo deal
    prodReadyToVal     <- mktZip modelParameters marketData product
    valProduct         <- valuateA isPricing (ProductReady2Val prodReadyToVal)
    viewerSAGreeks     <- mapG modelParameters marketData valProduct (ProductReady2Val prodReadyToVal)
    viewerSwap         <- mapV valProduct (ProductReady2Val prodReadyToVal)
    let valueViewer     = ValueViewer viewerSwap viewerSAGreeks
    let jsonOutput      = encodeJSON valueViewer 
    -- return (encodeJSON $ rvDeal $ prodReadyToVal)
    return jsonOutput

    
data ValueViewer = ValueViewer{ valueResult :: ValueContainer, greeksResult :: GreeksContainer} deriving (Eq, Show, Data, Typeable)   

data ValueContainer = ValueSwap { valueSwap :: Double, 
                                  valueLeg1 :: Double,
                                  valueLeg2 :: Double,
                                  valueCpnsLeg1 :: [Double],
                                  valueCpnsLeg2 :: [Double]                            
                        }
                    | ValueOption { valueOption :: Double,
                                    valueCpns :: [Double]                            
                        } deriving (Eq, Show, Data, Typeable)   
    
mapV :: ValueStorage -> ProductReady2Val -> Result ValueContainer   
mapV vS (ProductReady2Val {getR2ValProduct = Ready2Val (Swap g l1 l2 af)}) = do
    return (ValueSwap (value vS) (value $ (subValues vS)!!0) (value $ (subValues vS)!!1) (fmap value (subValues ((subValues vS)!!0))) (fmap value (subValues ((subValues vS)!!1))))
mapV vS ProductReady2Val {getR2ValProduct = Ready2Val (Option g l af)} = do
    return (ValueOption (value vS) (fmap value (subValues ((subValues vS)!!0))) )
mapV vS x = Error "Error in show result."    
    

