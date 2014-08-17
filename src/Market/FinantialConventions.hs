{-# LANGUAGE DeriveDataTypeable #-}
module Market.FinantialConventions   
    ( 
     FracConvention (..), CoumpondingConvention (..), BuySell (..), PayerReceiver (..), Convention (..),
     RateNature (..), StartDelay (..), ScheduleGenerator (..), Period (..), Tenor (..), Index (..)
    ) where
    
import Utils.MyJSON

-- Some finantial conventions
data FracConvention = ACTACT 
                    | ACT360 
                    | ACT365 
                    | THIRTY360 deriving (Eq, Show, Data, Typeable, Read)
                    
data CoumpondingConvention = LIN 
                           | YIELD deriving (Eq, Show, Data, Typeable, Read)
                           
data BuySell = BUY 
             | SELL deriving (Eq, Show, Data, Typeable, Read)
             
data PayerReceiver = PAYER 
                   | RECEIVER deriving (Eq, Show, Data, Typeable, Read)
                   
type Convention = (CoumpondingConvention, FracConvention)

data RateNature = Standard 
                | SwapRate deriving (Eq, Show, Data, Typeable, Read)
                
data StartDelay = OpenDays{nDays :: Int} deriving (Eq, Show, Data, Typeable, Read)

data ScheduleGenerator = ModFol{
                                   sgUnits :: Int, 
                                   sgPeriod :: Period
                               } deriving (Eq, Show, Data, Typeable, Read)

data Period = Day 
            | Week 
            | Month 
            | Year deriving (Eq, Show, Data, Typeable, Read)
            
data Tenor = Tenor {
                       tUnits :: Int, 
                       tPeriod :: Period
                   } deriving (Eq, Show, Data, Typeable, Read)

data Index = Index {
                       iName :: String, 
                       iDescription :: String, 
                       iRateNaure :: RateNature, 
                       iTenor :: Tenor,
                       iStartDelay :: StartDelay, 
                       iConvention :: Convention, 
                       iScheduleGenerator :: ScheduleGenerator
                   } deriving (Eq, Show, Data, Typeable, Read)
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   