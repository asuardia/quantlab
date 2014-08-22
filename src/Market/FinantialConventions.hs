{-# LANGUAGE DeriveDataTypeable #-}
module Market.FinantialConventions   
    ( 
     FracConvention (..), CoumpondingConvention (..), BuySell (..), PayerReceiver (..), Convention (..), Generator (..),
     RateNature (..), StartDelay (..), ScheduleGenerator (..), Period (..), TimeStep (..), Index (..),      LegGenerator (..),
     ProductTemplate (..), LegTemplate (..),
     PayFreq (..), ModPay (..), ModFix (..), Calendar (..), Schedules (..), BusinessDayConvention (..)   
    ) where
    
import Data.Time.Calendar    
import Utils.MyJSON
import Market.Currencies

--------------------------------------------------------------------------------------
-- Some finantial conventions
data FracConvention = ACTACT 
                    | ACT360 
                    | ACT365 
                    | THIRTY360 deriving (Eq, Show, Data, Typeable, Read)
                    
--------------------------------------------------------------------------------------
data CoumpondingConvention = LIN 
                           | YIELD deriving (Eq, Show, Data, Typeable, Read)
                           
--------------------------------------------------------------------------------------
data BuySell = BUY 
             | SELL deriving (Eq, Show, Data, Typeable, Read)
             
--------------------------------------------------------------------------------------
data PayerReceiver = PAYER 
                   | RECEIVER deriving (Eq, Show, Data, Typeable, Read)
                   
--------------------------------------------------------------------------------------
type Convention = (CoumpondingConvention, FracConvention)

--------------------------------------------------------------------------------------
data RateNature = Standard 
                | SwapRate deriving (Eq, Show, Data, Typeable, Read)
                
--------------------------------------------------------------------------------------
data StartDelay = OpenDays{nDays :: Int} deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------------------
data ScheduleGenerator = ModFol{
                                   sgUnits :: Int, 
                                   sgPeriod :: Period
                               } deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------------------
data Period = Day 
            | Week 
            | Month 
            | Year deriving (Eq, Show, Data, Typeable, Read)
            
--------------------------------------------------------------------------------------
data TimeStep = TimeStep {
                       tUnits :: Int, 
                       tPeriod :: Period
                   } deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------------------
data Index = Index {
                       iName :: String, 
                       iDescription :: String, 
                       iRateNaure :: RateNature, 
                       iTenor :: TimeStep,
                       iStartDelay :: StartDelay, 
                       iConvention :: Convention, 
                       iScheduleGenerator :: ScheduleGenerator
                   } deriving (Eq, Show, Data, Typeable, Read)
                   
--------------------------------------------------------------------------------------

-- Generators
data Generator = SwapGenerator   {
                                     legGenerator1 :: LegGenerator,
                                     legGenerator2 :: LegGenerator
                                 }     
               | OptionGenerator {
                                     legGenerator1 :: LegGenerator               
                                 } deriving (Eq, Show, Data, Typeable)    
--------------------------------------------------------------------------------------
                                 
data LegGenerator = LegFixGen    {
                                       lCurr :: Currency,
                                       lPayFreq :: PayFreq
                                   }     
                  | LegFloatGen {
                                       lCurr :: Currency,
                                       lIndex :: Index,
                                       lPayCalendar  :: Calendar,
                                       lFixCalendar  :: Calendar,
                                       lSchedules :: Schedules        
                                   } deriving (Eq, Show, Data, Typeable)    
--------------------------------------------------------------------------------------
data ProductTemplate = SwapTemplate { prGenerator :: Generator,
                                      prStartDay :: Day,
                                      prMat :: Int,
                                      legTemplate1 :: LegTemplate,
                                      legTemplate2 :: LegTemplate
                                    }
                     | OptionTemplate { prGenerator :: Generator,
                                        prStartDay :: Day,
                                        prMat :: Int,
                                        legTemplate1 :: LegTemplate
                                    } deriving (Eq, Show, Data, Typeable)    
-------------------------------------------------------------------------------------- 
data LegTemplate = LegFixTempl    {
                                       lPayRec  :: PayerReceiver, 
                                       lFixRate :: Double,
                                       lModPay  :: ModPay,
                                       lCon :: Convention
                                   }     
                  | LegFloatTempl {
                                       lPayRec  :: PayerReceiver, 
                                       lModFixing :: ModFix,
                                       lModPay  :: ModPay,
                                       lCon :: Convention,
                                       lMargin :: Double
                                   } deriving (Eq, Show, Data, Typeable)
-------------------------------------------------------------------------------------- 
data PayFreq = Annually | SemiAnnually | Quarterly | Monthly deriving (Eq, Show, Data, Typeable)   
--------------------------------------------------------------------------------------    
data ModPay = PayUpFront | PayInArrears deriving (Eq, Show, Data, Typeable)   
--------------------------------------------------------------------------------------    
data ModFix = FixUpFront | FixInArrears deriving (Eq, Show, Data, Typeable)   
--------------------------------------------------------------------------------------    
data Calendar = Target deriving (Eq, Show, Data, Typeable)          
--------------------------------------------------------------------------------------    
data Schedules = Schedules {
                              sTimeStep :: TimeStep,
                              sBusinessDayCon :: BusinessDayConvention
                           }deriving (Eq, Show, Data, Typeable)         
--------------------------------------------------------------------------------------    
data BusinessDayConvention = Following 
        | ModifiedFollowing 
        | Preceding
        | ModifiedPreceding
        | Unadjusted
        deriving (Eq, Show, Data, Typeable)                    
                   
                   
                   
                   
                   
                   
                   
                   