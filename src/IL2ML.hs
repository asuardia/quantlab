
import System.IO  
import System.Environment
import Vanilla.Types
import Utils.MyJSON
import Utils.PaintML
import Interfaces.ILInterface

 
main = do
       
    args               <- getArgs
    let ddbb            = args !! 0
    jsonInput          <- buildJSON ddbb    
    let input           = decodeJSON jsonInput :: Input
    let deal            = inputDeal input
    let product         = dealProduct deal
    let marketData      = inputMarket input
    let modelParameters = inputModelParams input
    -- Process
    let mktDatML        = paint2ML marketData
    let prodML          = paint2ML product
    
    writeFile "../logs/input.json" jsonInput
    writeFile "../logs/mktData.m" (prodML ++ mktDatML)
    return ()


