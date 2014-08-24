{-# LANGUAGE DeriveDataTypeable #-}
module Market.MarketData   
    ( 
     MarketData (..),           RateCurve (..),        CapFloorVol (..),          SwaptionVol (..),
     CapFloorVolGenerator (..), VolInterpolation (..), SwaptionVolGenerator (..), HistoricFixings (..)
    ) where

--import Data.Map    
import Data.Time.Calendar
import Utils.MyJSON
import Market.FinantialConventions
import Market.Currencies

-- Market Data
  
--------------------------------------------------------------------------------------
data MarketData = MarketData {
                                 curves       :: [RateCurve], 
                                 capFloorVols :: [CapFloorVolGenerator],
                                 swaptionVols :: [SwaptionVolGenerator],
                                 --historicFix  :: Map String HistoricFixings
                                 historicFix  :: [HistoricFixings]
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
data CapFloorVolGenerator = CFVInterpolator {
                                                cfIndex         :: String,
                                                cfInterpolation :: VolInterpolation,
                                                cfVols          :: CapFloorVol
                                            } 
                          | CFVGeneratorSABR deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------------------
data VolInterpolation = LINEAR_INT
                      | SABR_INT
                      | SABRRBS2_INT deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------------------
data CapFloorVol = CapFloorVol {
                                   cfvMatrix  :: [[Double]],
                                   cfvOptMat  :: [Day], 
                                   cfvStrikes :: [Double]
                               } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------------------
data SwaptionVolGenerator = SwVInterpolator {
                                             swCurr          :: Currency, 
                                             swInterpolation :: VolInterpolation,
                                             swVols          :: SwaptionVol
                                            } 
                          | SwVGeneratorSABR deriving (Eq, Show, Data, Typeable)
  
--------------------------------------------------------------------------------------
data SwaptionVol = SwaptionVol {
                                   swMatrix  :: [[[Double]]],
                                   swOptMat  :: [Day], 
                                   swStrikes :: [Double], 
                                   swSwapMat :: [[Int]]
                               } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------------------
--type HistoricFixings = Map Day Double
data HistoricFixings = HistoricFixings {
                                        hfIndex   :: String,
                                        hfDates   :: [Day],
                                        hfValues  :: [Double]
                                       } deriving (Eq, Show, Data, Typeable)
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
