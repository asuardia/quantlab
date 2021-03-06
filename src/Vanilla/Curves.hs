module Vanilla.Curves   
    ( 
     interpolateDFCurve, calcForward, calcForwardCMS
    ) where
    
import Data.Time.Calendar
import Utils.MyJSON
import Utils.MyUtils
import Market.MarketData
import Market.FinantialConventions
import Market.YearFractions
import Math.Interpolation
 
-------------------------------------------------------------------------- 
interpolateDFCurve :: RateCurve -> [Day] -> Result [Double] 
interpolateDFCurve    curve        days   = Ok values 
    where 
          values = fmap (interp1 x y) x0
          x      = fmap (fromIntegral . toModifiedJulianDay) 
                        (pillarMaturities curve)
          y      = discountFactors curve
          x0     = fmap (fromIntegral . toModifiedJulianDay) days
 
-------------------------------------------------------------------------- 
calcForward :: RateCurve -> Day -> Day -> FracConvention 
            -> Result (Double, [Double], [Double])
calcForward    curve        start  end    fracConv        
             = Ok (forward, [fd1, fd2], [deltaT])  
    where 
          forward = (fd1 - fd2)/(fd2 * deltaT)
          x       = fmap (fromIntegral . toModifiedJulianDay) 
                         (pillarMaturities curve)
          y       = discountFactors curve
          fd1     = interp1 x y (fromIntegral . toModifiedJulianDay $ start)
          fd2     = interp1 x y (fromIntegral . toModifiedJulianDay $ end)
          deltaT  = calcYearFrac start end fracConv
-------------------------------------------------------------------------- 
          
calcForwardCMS :: RateCurve -> [Day] -> FracConvention 
               -> Result (Double, [Double], [Double])
calcForwardCMS    curve        cmsDates fracConv        
                = Ok (forward, fds, deltaT)
    where 
          forward = ((head fds) - (last fds))/annuity
          x       = fmap (fromIntegral . toModifiedJulianDay) 
                         (pillarMaturities curve)
          y       = discountFactors curve
          fds     = fmap (interp1 x y) 
                         (fmap (fromIntegral . toModifiedJulianDay) cmsDates)
          deltaT  = zipWith (flip123_312 calcYearFrac fracConv) 
                            (init cmsDates) (tail cmsDates)
          annuity = sum $ zipWith (*) (tail fds) deltaT