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

interpolateVol :: CapFloorVol -> Day -> Double -> Result Double 
interpolateVol    volCF          day    strike  = Ok value 
    where 
          value = interp2 x y z x0 y0
          x     = fmap (fromIntegral . toModifiedJulianDay) (cfvOptMat volCF)
          y     = cfvStrikes volCF
          z     = cfvMatrix volCF
          x0    = (fromIntegral . toModifiedJulianDay) day
          y0    = strike

