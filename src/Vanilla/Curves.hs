module Vanilla.Curves   
    ( 
     interpolateDFCurve, 
     calcForward
    ) where
    
import Data.Time.Calendar
import Utils.MyJSON
import Market.MarketData
import Market.FinantialConventions
import Market.YearFractions
import Math.Interpolation

interpolateDFCurve :: RateCurve -> [Day] -> Result [Double] 
interpolateDFCurve    curve        days   = Ok values 
    where 
          values = fmap (interp1 x y) x0
          x      = fmap (fromIntegral . toModifiedJulianDay) (pillarMaturities curve)
          y      = discountFactors curve
          x0     = fmap (fromIntegral . toModifiedJulianDay) days

calcForward :: RateCurve -> Day -> Day -> FracConvention -> Result Double 
calcForward    curve        start  end    fracConv        = Ok forward 
    where 
          forward = (fd1 - fd2)/(fd2 * deltaT)
          x       = fmap (fromIntegral . toModifiedJulianDay) (pillarMaturities curve)
          y       = discountFactors curve
          fd1     = interp1 x y (fromIntegral . toModifiedJulianDay $ start)
          fd2     = interp1 x y (fromIntegral . toModifiedJulianDay $ end)
          deltaT  = calcYearFrac start end fracConv