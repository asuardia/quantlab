module Market.YearFractions   
    ( 
     calcYearFrac
    ) where
    
import Data.Time.Calendar
import Market.FinantialConventions


--------------------------------------------------------------------------
calcYearFrac :: Day -> Day -> FracConvention -> Double
calcYearFrac    d1     d2     ACTACT         =  (fromIntegral $ (toModifiedJulianDay d2) - (toModifiedJulianDay d1))/360
calcYearFrac    d1     d2     ACT360         =  (fromIntegral $ (toModifiedJulianDay d2) - (toModifiedJulianDay d1))/360
calcYearFrac    d1     d2     ACT365         =  (fromIntegral $ (toModifiedJulianDay d2) - (toModifiedJulianDay d1))/360
calcYearFrac    d1     d2     THIRTY360      =  (fromIntegral $ (toModifiedJulianDay d2) - (toModifiedJulianDay d1))/360