{-# LANGUAGE DeriveDataTypeable #-}
module Vanilla.Process
    ( 
     process 
    ) where 

import Utils.MyJSON
import Vanilla.Types
import Vanilla.Instances
import Vanilla.Containers

--------------------------------------------------------------------------
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
    valProduct         <- valuateA isPricing prodReadyToVal
    viewerSAGreeks     <- mapG modelParameters marketData 
                               valProduct      prodReadyToVal
    viewerVal          <- mapV valProduct prodReadyToVal
    let viewer          = Viewer viewerVal viewerSAGreeks
    let jsonOutput      = encodeJSON viewer 
    --return (encodeJSON marketData)
    return jsonOutput

    

    

