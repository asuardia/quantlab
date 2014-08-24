{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
 
module Market.Generators   
    ( 
     Generator (..), LegGenerator (..), ProductTemplate (..), LegInfo (..),
     euribor3mSwGen, builder,
     liborPO,
     forwardMO,
    ) where 
    
import Data.Time.Calendar        
import Vanilla.Types
import Market.FinantialConventions
import Market.Indexes



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
                                       lSchedules :: Schedules
                                   }     
                  | LegFloatGen {
                                       lCurr :: Currency,
                                       lIndex :: Index,
                                       lPayCalendar  :: Calendar,
                                       lFixCalendar  :: Calendar,
                                       lSchedules :: Schedules        
                                   } deriving (Eq, Show, Data, Typeable)    
--------------------------------------------------------------------------------------
data ProductTemplate = SwapTemplate { 
                                      prCapital :: Double,
                                      prGenerator :: Generator,
                                      prStartDay :: Day,
                                      prMat :: Int,
                                      legInfo1 :: LegInfo,
                                      legInfo2 :: LegInfo
                                    }
                     | OptionTemplate { 
                                        prCapital :: Double,
                                        prGenerator :: Generator,
                                        prStartDay :: Day,
                                        prMat :: Int,
                                        legInfo1 :: LegInfo
                                    } deriving (Eq, Show, Data, Typeable)    
-------------------------------------------------------------------------------------- 
data LegInfo = LegFixInfo   {
                                lPayRec  :: PayerReceiver, 
                                lFixRate :: Double,
                                lModPay  :: ModPay,
                                lCon :: Convention,
                                lDiscCurve :: String
                            }     
             | LegFloatInfo {
                                lPayRec  :: PayerReceiver, 
                                lModFixing :: ModFix,
                                lModPay  :: ModPay,
                                lCon :: Convention,
                                lMargin :: Double,
                                lPayOff :: PayOff,
                                lModel :: Model,
                                lEstCurve :: String,
                                lDiscCurve :: String
                            } deriving (Eq, Show, Data, Typeable)


--------------------------------------------------------------------------------------
euribor3mSwGen = SwapGenerator{
                                  legGenerator1 = LegFixGen   {
                                                                  lCurr      = EUR,
                                                                  lSchedules = Schedules {
                                                                                             sTimeStep = TimeStep {
                                                                                                                      tUnits  = 12, 
                                                                                                                      tPeriod = Month
                                                                                                                  },
                                                                                             sBusinessDayCon = ModifiedFollowing
                                                                                         }
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
                              
--------------------------------------------------------------------------------------
liborPO = Libor (ModifiedJulianDay 0) (ModifiedJulianDay 0) (ModifiedJulianDay 0) (ModifiedJulianDay 0) (LIN, ACT360) 0.0

--------------------------------------------------------------------------------------
forwardMO = Forward (ModifiedJulianDay 0) 0.0

--------------------------------------------------------------------------------------
builder :: ProductTemplate      -> Result Product
builder    swt@SwapTemplate   {} = swapBuilder swt 
builder    opt@OptionTemplate {} = optionBuilder opt 
--------------------------------------------------------------------------------------
swapBuilder :: ProductTemplate               -> Result Product
swapBuilder    swt@(SwapTemplate c swg sd mt li1 li2)  = do
    leg1 <- legBuilder (legGenerator1 swg) li1 sd mt c
    leg2 <- legBuilder (legGenerator2 swg) li2 sd mt c
    return (Swap leg1 leg2 (AddFlows []))
swapBuilder    t                           = Error "Can't build product."           
--------------------------------------------------------------------------------------
optionBuilder :: ProductTemplate               -> Result Product
optionBuilder    opt@(OptionTemplate c opg sd mt li1)  = do
    leg1 <- legBuilder (legGenerator1 opg) li1 sd mt c
    return (Option leg1 (AddFlows []))
optionBuilder    t                           = Error "Can't build product."           
--------------------------------------------------------------------------------------       

legBuilder :: LegGenerator                                            -> LegInfo                               -> Day -> Int -> Double -> Result Leg
legBuilder    (LegFixGen c (Schedules (TimeStep u Month) bdc))           (LegFixInfo pr fr mp cn dc)              sD     ys     rc      = do
    let periods = [1 .. (floor $ (fromIntegral ys) * (12.0 / (fromIntegral u)))]
    let cpns    = buildCoupons periods
    return FixedLeg {coupons = cpns, discCurve = dc, legPayerReceiver = pr}
        where 
              buildCoupons []        = []
              buildCoupons (f:r) = coupon : buildCoupons r
                  where 
                        coupon = Fixed {cpStartDate  = srtCpn, cpEndDate = endCpn, cpPayDate    = endCpn,  cpYearFrac  = 0.0, cpRemainingCapital = rc, 
                                        cpConvention = cn,     fxRate    = fr,     fxDiscFactor = 0.0,     cpDiscCurve = dc} 
                        srtCpn = am f sD
                        endCpn = am (f+1) sD
                       
      
legBuilder    (LegFloatGen c i pc fc (Schedules (TimeStep u Month) bdc)) (LegFloatInfo pr mf mp cn m po mo ec dc) sD     ys     rc      = do
    let periods = [1 .. (floor $ (fromIntegral ys) * (12.0 / (fromIntegral u)))]
    let cpns    = buildCoupons periods
    return VariableLeg {coupons = cpns, discCurve = dc, legIndex = iName i, legPayerReceiver = pr}
        where 
              buildCoupons []        = []
              buildCoupons (f:r) = coupon : buildCoupons r
                  where 
                        coupon = Variable {cpStartDate  = srtCpn, cpEndDate = endCpn,    cpPayDate = endCpn, cpYearFrac = 0.0, cpRemainingCapital = rc, 
                                           cpConvention = cn,     varPayOff = newPayOff, varModel  = mo,     varNum0   = 0.0,  cpDiscCurve        = dc, cpIndex = iName i}
                        newPayOff = payOffBuilder po srtCpn f
                        srtCpn = am f sD
                        endCpn = am (f+1) sD
           
--------------------------------------------------------------------------------------  
payOffBuilder :: PayOff -> Day -> Integer -> PayOff
payOffBuilder    (Libor lF lS lE lP lC m) sC period = Libor (addDays (-2) sC) (am period sC) (am (period+1) sC) (am (period+1) sC) lC m
payOffBuilder    (Caplet lF lS lE lP lC m str) sC period = Caplet (addDays (-2) sC) (am period sC) (am (period+1) sC) (am (period+1) sC) lC m str
payOffBuilder    (Floorlet lF lS lE lP lC m str) sC period = Floorlet (addDays (-2) sC) (am period sC) (am (period+1) sC) (am (period+1) sC) lC m str



am = addGregorianMonthsClip


















    