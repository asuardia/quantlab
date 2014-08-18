module Vanilla.Volatility   
    ( 
     interpolateVol
    ) where
    
import Data.Time.Calendar
import Utils.MyJSON
import Market.MarketData
import Market.FinantialConventions
import Market.YearFractions
import Math.Interpolation

interpolateVol :: CapFloorVolGenerator -> Day -> Double -> Result Double 
interpolateVol    volCFGen                day    strike  = Ok value 
    where 
          value = interp2 x y z x0 y0
          volCF = cfVols volCFGen
          x     = fmap (fromIntegral . toModifiedJulianDay) (cfvOptMat volCF)
          y     = cfvStrikes volCF
          z     = cfvMatrix volCF
          x0    = (fromIntegral . toModifiedJulianDay) day
          y0    = strike

