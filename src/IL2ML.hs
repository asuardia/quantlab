import System.IO  
import Vanilla.Types
import Utils.PaintML
import Interfaces.ILInterface

 
main = do
    -- This is your data source
    --let ddbb = "c:/Projects/Inputs/7023936.in"
    --let ddbb = "c:/Projects/Inputs/7023936_fixed.in"
    --let ddbb = "c:/Projects/Inputs/6435516_cap.in"
    let ddbb = "../../Inputs/4536386_sw_cancel.in"
    
    -- You know what product you want, you have different builders
    jsonInput <- buildJSON ddbb
    writeFile "../../Logs/input.json" jsonInput
    
    let input = decodeJSON jsonInput :: Input
    let deal = inputDeal input
    let product = dealProduct deal
    let marketData = inputMarket input
    let modelParameters = inputModelParams input
    -- Process
    let mktDatML = paint2ML marketData
    let prodML = paint2ML product
    
    writeFile "../../Logs/mktData.m" (prodML ++ mktDatML)
    return ()


