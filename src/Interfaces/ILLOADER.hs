import System.IO  
import System.Environment
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types
import Vanilla.Types
import Interfaces.ILInterface
import Vanilla.Process


 
main = do
   
    args       <- getArgs
    let ddbb    = args !! 0
    let output  = args !! 1
    jsonInput  <- buildJSON ddbb
    --writeFile "../../logs/input.json" jsonInput
    ------------------------------------------------------------------
    ------------------------------------------------------------------
    ------------------------------------------------------------------
    -- Process
    let rJsonOutput = process jsonInput
    let jsonOutput  = get rJsonOutput
    --writeFile "../../logs/output.json" jsonOutput
    writeFile output jsonOutput        
    return ()
    where get (Ok x)    = x
          get (Error x) = x



          