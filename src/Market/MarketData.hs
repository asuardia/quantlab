{-# LANGUAGE DeriveDataTypeable #-}
module Market.MarketData   
    ( 
     MarketData (..), RateCurve (..), CapFloorVol (..), SwaptionVol (..)
    ) where
    
import Data.Time.Calendar
import Utils.MyJSON
import Market.FinantialConventions
import Market.Currencies

-- Market Data
  
--------------------------------------------------------------------------------------
data MarketData = MarketData {
                                 curves       :: [RateCurve], 
                                 capFloorVols :: [CapFloorVol],
                                 swaptionVols :: [SwaptionVol]
                             } deriving (Eq, Show, Data, Typeable)
  
--------------------------------------------------------------------------------------
data RateCurve = RateCurve {
                               curveName            :: String, 
                               curveIndex           :: Maybe String,
                               currencyCurve        :: Currency,
                               conventionCurve      :: Convention, 
                               baseCurve            :: String,
                               discountFactors      :: [Double], 
                               interpolationFormula :: String,
                               pillarMaturities     :: [Day], 
                               refDate              :: Day, 
                               spreadCurveFormula   :: String, 
                               value2Interpolate    :: String
                           } deriving (Eq, Show, Data, Typeable)
  
--------------------------------------------------------------------------------------
data CapFloorVol = CapFloorVol {
                                   cfvIndex   :: String, 
                                   cfvMatrix  :: [[Double]],
                                   cfvOptMat  :: [Day], 
                                   cfvStrikes :: [Double]
                               } deriving (Eq, Show, Data, Typeable)
  
--------------------------------------------------------------------------------------
data SwaptionVol = SwaptionVol {
                                   swCurr    :: Currency, 
                                   swMatrix  :: [[[Double]]],
                                   swOptMat  :: [Day], 
                                   swStrikes :: [Double], 
                                   swSwapMat :: [[Int]]
                               } deriving (Eq, Show, Data, Typeable)

