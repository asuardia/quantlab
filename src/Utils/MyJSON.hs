{-# LANGUAGE DeriveDataTypeable #-}
module Utils.MyJSON   
    ( 
     getObjectJS, getArrayObjectJS, getValueJS, getArrayJS, getTryValueJS,
     getErr,      error2EmptyList,  checkAllOk, getOnlyOk,  myEncodeJSON,
     module Text.JSON, 
     module Text.JSON.Generic, 
     module Text.JSON.Types
    ) where
    
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types
import Control.Monad 
import Utils.MyUtils   

    
--------------------- MyJSON ---------------------------------------------
--------------------------------------------------------------------------
listFunctions :: JSON a => String -> [JSObject JSValue -> Result a]
listFunctions              s       = fmap valFromObj list
    where 
          list = reverse $ split s '/'       
--------------------------------------------------------------------------    
listInitFunctions :: JSON a => String -> [JSObject JSValue -> Result a]
listInitFunctions              s       = fmap valFromObj list
    where 
          list = tail $ reverse $ split s '/'       
--------------------------------------------------------------------------
lastFunction :: JSON a => String -> JSObject JSValue -> Result a     
lastFunction              s       = valFromObj lastFun
    where 
          lastFun = last $ split s '/'       
--------------------------------------------------------------------------
composedFunctions :: JSON a => [a -> Result a] -> a -> Result a
composedFunctions              []                   =  return 
composedFunctions              lf                   =  (foldr (<=<) return lf)
-------------------------------------------------------------------------- 
getObjectJS :: JSObject JSValue -> String -> Result (JSObject JSValue)
getObjectJS    rJO                 st      = 
    (return rJO) >>= composedFunctions (listFunctions st)
--------------------------------------------------------------------------
getArrayObjectJS :: JSON a => 
                    JSObject JSValue -> String -> Result a
getArrayObjectJS    rJO                 st      =  
    (return rJO) >>= composedFunctions listFun >>= lastFunction st
    where 
          listFun = listInitFunctions st
--------------------------------------------------------------------------
getValueJS :: JSON a => JSObject JSValue -> String -> Result a
getValueJS                                          = getArrayObjectJS    
--------------------------------------------------------------------------
getArrayJS :: JSON a => JSObject JSValue -> String -> Result a
getArrayJS                                          = getArrayObjectJS
--------------------------------------------------------------------------
getTryValueJS :: JSON a => JSObject JSValue -> [String] 
                        -> Result a
getTryValueJS              jsO                 ks 
                        =  if ((length newList) == 0) 
                           then Error ("Doesn´t find any of theese keys:" ++ show ks) 
                           else (newList!!0)
    where 
          list                   = fmap (getValueJS jsO) ks
          newList                = filter filterResult list
          filterResult (Error a) = False
          filterResult x         = True
--------------------------------------------------------------------------
getErr :: Result a  -> String
getErr    (Error s) =  s
getErr    x         =  ""
--------------------------------------------------------------------------  
myEncodeJSON :: Data a => Result a -> String
myEncodeJSON             (Ok x)    =  encodeJSON x
myEncodeJSON             (Error x) =  x
--------------------------------------------------------------------------
error2EmptyList :: Result a  -> [Result a]
error2EmptyList    (Error x) =  []
error2EmptyList    x         =  [x]
--------------------------------------------------------------------------
checkAllOk :: [Result a] -> Result [a]
checkAllOk    x 
    | all check x        =  Ok (fmap (\(Ok a) -> a) x)
    | otherwise          =  Error (concat $ fmap getErr x)
        where 
              check :: Result a -> Bool
              check    (Ok a)   =  True
              check    x        =  False 
-------------------------------------------------------------------------- 
getOnlyOk :: [Result a] -> Result [a]
getOnlyOk    x = checkAllOk (filter check x)
        where 
              check :: Result a -> Bool
              check    (Ok a)   =  True
              check    x        =  False


