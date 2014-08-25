{-# LANGUAGE DeriveDataTypeable #-}
module Vanilla.ModelParameters   
    ( 
     ModelParameters (..), Parameters (..), ParamsData (..)
    ) where
    
import Data.Time.Calendar
import Utils.MyJSON
import Market.FinantialConventions


-------------------------------------------------------------------------- 
-- Model Parameters

data ModelParameters = NoParams 
                     | ModelParameters { 
                                            parameters :: [Parameters]
                                       } deriving (Eq, Show, Data, Typeable)

-------------------------------------------------------------------------- 
-- Parameters

data Parameters = SABR { 
                            paramsIndex  :: String, 
                            vatmSABR     :: ParamsData, 
                            betaSABR     :: ParamsData, 
                            rhoSABR      :: ParamsData, 
                            volOfVolSABR :: ParamsData
                       } 
                | RBS2 { 
                            paramsIndex :: String, 
                            rightStrike :: ParamsData, 
                            rightParams :: ParamsData,
                            leftStrike  :: ParamsData, 
                            leftParams  :: ParamsData
                        } 
                | KAPPA {   paramsIndex :: String, 
                            kTenors     :: [Double], 
                            kValues     :: [Double]
                        } deriving (Eq, Show, Data, Typeable)

-------------------------------------------------------------------------- 

data ParamsData = ParamsMatrix { 
                                    expiries :: [Double], 
                                    tenors   :: [Double], 
                                    matrix   :: [[Double]]
                               } 
                | ParamsVector {    
                                    expiries :: [Double], 
                                    values   :: [Double]
                               } deriving (Eq, Show, Data, Typeable)