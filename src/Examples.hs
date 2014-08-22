import System.IO  
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types

import Interfaces.ILInterface
import Vanilla.Types
import Vanilla.Instances
import Vanilla.SAGreeks
import Vanilla.Viewer
import Market.Generators

main = do
    let ddbb  = "../inputs/market.in"
    marketJSON <- buildMarketJSON ddbb
    writeFile "../logs/market.json" marketJSON
    
    let marketData           = decodeJSON marketJSON :: MarketData
    --let modelParameters = buildModelParams
    
    
    let product1         = "" --buildProduct
    --let product2         = buildProduct
    --let product3         = buildProduct
    --let portfolio  = [product1, product2, product3]
    --let modelParameters = buildModelParams
    --viewer = concat $ fmap () portfolio
    --let jsonOutput      = encodeJSON viewer    
    --writeFile "../logs/output.json" jsonOutput        
    writeFile "../logs/output.json" product1
    return ()
          
{-          
processProduct :: ModelParameters -> MarketData -> Product -> Result Viewer
processProduct    modelParameters    marketData    product  = do
    let isPricing   = True
    prodReadyToVal <- mktZip modelParameters marketData product
    valProduct     <- valuateA isPricing (ProductReady2Val prodReadyToVal)
    viewerSAGreeks <- mapG modelParameters marketData valProduct (ProductReady2Val prodReadyToVal)
    viewerVal      <- mapV valProduct (ProductReady2Val prodReadyToVal)
    return (Viewer viewerVal viewerSAGreeks)-}