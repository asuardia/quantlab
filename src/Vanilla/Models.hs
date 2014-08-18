{-# LANGUAGE DeriveDataTypeable #-}
module Vanilla.Models   
    ( 
     Model (..)
    ) where

import Utils.MyJSON
import Market.FinantialConventions 
 
-- Models
data Model =  Forward            {
                                  forward :: Double
                                 } 
           |  ForwardNonStandard {
                                  forward         :: Double, 
                                  sigmaAdjustment :: Double
                                 }
           |  Black              {
                                  blackSigma :: Double, 
                                  forward    :: Double
                                 }
           |  BlackNonStandard   {
                                  blackSigma      :: Double, 
                                  forward         :: Double, 
                                  sigmaAdjustment :: Double
                                 }
           |  HaganRepSABRRBS2   {
                                  forward  :: Double, 
                                  vAtm     :: Double, 
                                  beta     :: Double, 
                                  rho      :: Double,
                                  volOfVol :: Double, 
                                  xPlus    :: Double, 
                                  xMinus   :: Double, 
                                  nu       :: Double, 
                                  mu       :: Double,
                                  kappa    :: Double
                                 } deriving (Eq, Show, Data, Typeable)
