{-# LANGUAGE DeriveDataTypeable #-}
module Vanilla.Viewer   
    ( 
     Viewer (..), ValueContainer (..), mapV     
    ) where 
    

import Data.Monoid   
import Vanilla.Types
import Vanilla.Instances
import Vanilla.SAGreeks    

--------------------------------------------------------------------------------------
data Viewer = Viewer { 
                         valueResult  :: ValueContainer, 
                         greeksResult :: GreeksContainer
                     } deriving (Eq, Show, Data, Typeable)   

--------------------------------------------------------------------------------------
data ValueContainer = ValueSwap   {
                                      valueProduct  :: Double, 
                                      valueLeg1     :: Double,
                                      valueLeg2     :: Double,
                                      valueCpnsLeg1 :: [Double],
                                      valueCpnsLeg2 :: [Double]                            
                                  }
                    | ValueOption { 
                                      valueProduct :: Double,
                                      valueCpns    :: [Double]
                                  }      
                    | Value       {
                                      valueProduct :: Double
                                  } deriving (Eq, Show, Data, Typeable)   
    
--------------------------------------------------------------------------------------
-- class mapV Ready2Val and Portfolio
mapV :: ValueStorage -> ProductReady2Val -> Result ValueContainer   
mapV    vS 
        ProductReady2Val {getR2ValProduct = Ready2Val (Swap g l1 l2 af)}
                                          = do
    return (ValueSwap 
                (value vS) 
                (value $ (subValues vS)!!0) 
                (value $ (subValues vS)!!1) 
                (fmap value (subValues ((subValues vS)!!0))) (fmap value (subValues ((subValues vS)!!1)))
           )
mapV    vS 
        ProductReady2Val {getR2ValProduct = Ready2Val (Option g l af)} 
                                          = do
    return (ValueOption 
                (value vS) 
                (fmap value (subValues ((subValues vS)!!0))) 
           )
mapV    vS              x                 = Error "Error in show result."        

--------------------------------------------------------------------------------------
instance Monoid ValueContainer where  
    mempty          = Value 0.0 
    mappend vC1 vC2 = Value ((valueProduct vC1) + (valueProduct vC2))
    

instance Monoid Viewer where  
    mempty          = Viewer mempty mempty 
    mappend vV1 vV2 = Viewer (mappend (valueResult vV1) (valueResult vV2)) (mappend (greeksResult vV1) (greeksResult vV2))  





    