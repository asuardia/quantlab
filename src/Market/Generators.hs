{-# LANGUAGE DeriveDataTypeable #-}
module Market.Generators   
    ( 
     euribor3mSwGen
    ) where 
    
import Vanilla.Types
import Market.FinantialConventions
import Market.Indexes

euribor3mSwGen = SwapGenerator{
                                  legGenerator1 = LegFixGen   {
                                                                  lCurr    = EUR,
                                                                  lPayFreq = Annually
                                                              } ,
                                  legGenerator2 = LegFloatGen {
                                                                  lCurr        = EUR,
                                                                  lIndex       = euribor3m,
                                                                  lPayCalendar = Target,
                                                                  lFixCalendar = Target,
                                                                  lSchedules   = Schedules {
                                                                                               sTimeStep       = TimeStep {
                                                                                                                               tUnits  = 3, 
                                                                                                                               tPeriod = Month
                                                                                                                          },
                                                                                               sBusinessDayCon = ModifiedFollowing
                                                                                   }        
                                                              }
                              }  
                              
swapBuilder :: ProductTemplate               -> Result Product
swapBuilder    swt@(SwapTemplate swg sd mt lt1 lt2)  = do
    leg1 <- legBuilder sd mt lt1
    leg2 <- legBuilder sd mt lt2
    return (Swap (Just swt) leg1 leg2 (AddFlows []))
swapBuilder    t                           = Error "Can't build product."                  

legBuilder :: Day -> Int -> LegTemplate -> Result Leg
legBuilder    sD     ys     lT           = Error ""
