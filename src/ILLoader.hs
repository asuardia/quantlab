import System.IO  
import System.Environment
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types
import Vanilla.Types
import Interfaces.ILInterface
import Vanilla.Process


 
main = do
    -- This is your data source
    --let ddbb  = "../inputs/7023936.in"
    --let ddbb  = "../inputs/7023936_fixed.in"
    --let ddbb  = "../inputs/6435516_cap.in"
    --let ddbb  = "../inputs/4536386_sw_cancel.in"    
    args <- getArgs
    let ddbb = args !! 0
    jsonInput <- buildJSON ddbb
    writeFile "../logs/input.json" jsonInput
    ------------------------------------------------------------------
    ------------------------------------------------------------------
    ------------------------------------------------------------------
    -- Process
    let rJsonOutput = process jsonInput
    let jsonOutput  = get rJsonOutput
    writeFile "../logs/output.json" jsonOutput        
    return ()
    where get (Ok x)    = x
          get (Error x) = x



          