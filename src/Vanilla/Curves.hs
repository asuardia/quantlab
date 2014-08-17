module Vanilla.Curves   
    ( 
     interpolateDFCurve
    ) where
    
import Data.Time.Calendar
import Utils.MyJSON
import Market.MarketData
import Math.Interpolation

interpolateDFCurve :: RateCurve -> [Day] -> Result [Double] 
interpolateDFCurve curve days = Ok values 
    where values = fmap (interp1 x y) x0
          x      = fmap (fromIntegral . toModifiedJulianDay) (pillarMaturities curve)
          y      = discountFactors curve
          x0     = fmap (fromIntegral . toModifiedJulianDay) days
