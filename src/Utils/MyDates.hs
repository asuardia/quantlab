module Utils.MyDates   
    ( 
     day2Excel,
     excel2Day,
     day2Matlab,
     matlab2Day
    ) where
    
import Data.Time.Calendar 
   
--------------------------------------------------------------------------------------
day2Excel :: Day -> Integer
day2Excel    day =  (toModifiedJulianDay day) - ((toModifiedJulianDay $ fromGregorian 1900 1 1) - 1)   
--------------------------------------------------------------------------------------
excel2Day :: Integer -> Day
excel2Day    xdate   =  ModifiedJulianDay (xdate + (toModifiedJulianDay $ fromGregorian 1900 1 1) - 1)   
--------------------------------------------------------------------------------------
day2Matlab :: Day -> Integer
day2Matlab    day =  (toModifiedJulianDay day) - ((toModifiedJulianDay $ fromGregorian 0 1 1))   
--------------------------------------------------------------------------------------
matlab2Day :: Integer -> Day
matlab2Day    mdate   =  ModifiedJulianDay (mdate + (toModifiedJulianDay $ fromGregorian 0 1 1) )
