{-# LANGUAGE DeriveDataTypeable #-}
module Vanilla.Models   
    ( 
     Model (..)
    ) where

import Utils.MyJSON
import Data.Time.Calendar
import Market.FinantialConventions 
-------------------------------------------------------------------------- 
-- Models
data Model =  Forward            {
                                  referenceDate :: Day,
                                  forward       :: (Double, [Day], [Double], [Double])
                                 } 
           |  ForwardNonStandard {
                                  referenceDate   :: Day,
                                  expiry          :: Double,
                                  time2Pay        :: Double,
                                  forward         :: (Double, [Day], [Double], [Double]), 
                                  sigmaAdjustment :: Double
                                 }
           |  Black              {
                                  referenceDate :: Day,
                                  expiry        :: Double,
                                  blackSigma    :: Double, 
                                  forward       :: (Double, [Day], [Double], [Double])
                                 }
           |  BlackNonStandard   {
                                  referenceDate   :: Day,
                                  expiry          :: Double,
                                  time2Pay        :: Double,
                                  blackSigma      :: Double, 
                                  forward         :: (Double, [Day], [Double], [Double]), 
                                  sigmaAdjustment :: Double
                                 }
           |  HaganRepSABRRBS2   {
                                  referenceDate :: Day,
                                  expiry        :: Double,
                                  forward       :: (Double, [Day], [Double], [Double]), 
                                  vAtm          :: Double, 
                                  beta          :: Double, 
                                  rho           :: Double,
                                  volOfVol      :: Double, 
                                  xPlus         :: Double, 
                                  xMinus        :: Double, 
                                  nu            :: Double, 
                                  mu            :: Double,
                                  kappa         :: Double
                                 } deriving (Eq, Show, Data, Typeable)
