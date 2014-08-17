import System.IO  
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types
import Vanilla.Types
import Interfaces.ILInterface
import Vanilla.Process


 
main = do
    -- This is your data source
    --let ddbb  = "c:/Projects/Inputs/7023936.in"
    --let ddbb  = "c:/Projects/Inputs/7023936_fixed.in"
    --let ddbb  = "c:/Projects/Inputs/6435516_cap.in"
    let ddbb  = "c:/Projects/Inputs/4536386_sw_cancel.in"
    jsonInput <- buildJSON ddbb
    writeFile "c:/Projects/Logs/input.json" jsonInput
    ------------------------------------------------------------------
    ------------------------------------------------------------------
    ------------------------------------------------------------------
    -- Process
    let rJsonOutput = process jsonInput
    let jsonOutput  = get rJsonOutput
    writeFile "c:/Projects/Logs/output.json" jsonOutput        
    return ()
    where get (Ok x)    = x
          get (Error x) = x


          