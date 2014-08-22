import System.IO  
import Vanilla.Types
import Utils.PaintML
import Interfaces.ILInterface

 
main = do
    -- This is your data source
    --let ddbb  = "../inputs/7023936.in"
    --let ddbb  = "../inputs/7023936_fixed.in"
    let ddbb  = "../inputs/6435516_cap.in"
    --let ddbb  = "../inputs/4536386_sw_cancel.in"
    
    -- You know what product you want, you have different builders
    jsonInput <- buildJSON ddbb
    writeFile "../logs/input.json" jsonInput
    
    let input = decodeJSON jsonInput :: Input
    let deal = inputDeal input
    let product = dealProduct deal
    let marketData = inputMarket input
    let modelParameters = inputModelParams input
    -- Process
    let mktDatML = paint2ML marketData
    let prodML = paint2ML product
    
    writeFile "../logs/mktData.m" (prodML ++ mktDatML)
    return ()


