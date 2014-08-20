module Vanilla.Process
    ( 
     process 
    ) where 

import Vanilla.Types
import Vanilla.Instances

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
    prodReadyToVal      <- mktZip modelParameters marketData product
    valProduct          <- valuateA isPricing (ProductReady2Val prodReadyToVal)
    let jsonOutput      = encodeJSON valProduct 
    -- return (encodeJSON $ rvDeal $ prodReadyToVal)
    return jsonOutput

    
    
    
    
    
    
    

