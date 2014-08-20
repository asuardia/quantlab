module Vanilla.MarketZip   
    ( 
     getCouponFMktModInfo, getCouponVMktModInfo
    ) where
    
import qualified Data.Map as Map
import Data.Data
import Data.Typeable
import Data.Time.Calendar
import Utils.MyJSON
import Vanilla.ModelParameters
import Vanilla.Models
import Vanilla.PayOffs
import Vanilla.Curves
import Vanilla.Volatility
import Market.FinantialConventions
import Market.YearFractions
import Market.MarketData
import Math.Interpolation

getCouponFMktModInfo :: ModelParameters -> MarketData -> String -> Day             
                     -> Day             -> Day        -> Convention 
                     -> Result (Double, Double)
getCouponFMktModInfo    modParams          mktData       discCurve startDate 
                        endDate            payDate       conv 
		              = do 
    let dCurve        = (filter (\cv -> (curveName cv) == discCurve) (curves mktData))!!0
    let yearFrac      = calcYearFrac startDate endDate (snd conv)
    interpPayDate    <- interpolateDFCurve dCurve [startDate]
    return	            (yearFrac, (interpPayDate!!0))
--------------------------------------------------------------------------------------
getCouponVMktModInfo :: ModelParameters      -> MarketData -> String     -> String -> Day 
                     -> Day                  -> Day        -> Convention 
                     -> PayOff 
                     -> Model 
                     -> Result (Double, Double, Model)
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        Libor   {liborFix = lF,  liborStart = lS, liborEnd = lE, liborPay = lP, liborConvention = lC, margin = m}     
                        Forward {forward  = fwd} 
                      = do
    (yearFrac, 
     interpPayDate,
     evDate,
     expiry,   
     calcFwd)        <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                              lF      lS        lE       (snd lC)
    return	            (yearFrac, interpPayDate, Forward evDate calcFwd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        Libor              {liborFix = lF,  liborStart      = lS,     liborEnd = lE, 
                                            liborPay = lP,  liborConvention = lC,     margin   = m}     
                        ForwardNonStandard {forward  = fwd, sigmaAdjustment = sigmaAd} 
                      = do
    (yearFrac, 
     interpPayDate, 
     evDate,
     expiry,   
     calcFwd)        <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                              lF      lS        lE       (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    calcSigmaAd      <- interpolateVol volCF lF calcFwd
    return	            (yearFrac, interpPayDate, ForwardNonStandard evDate expiry calcFwd calcSigmaAd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        Caplet {liborFix        = lF,  liborStart = lS,   liborEnd  = lE, liborPay = lP, 
                                liborConvention = lC,  margin     = m,    capStrike = str}     
                        Black  {forward         = fwd, blackSigma = sigma} 
                      = do
    (yearFrac, 
     interpPayDate, 
     evDate,
     expiry,   
     calcFwd)        <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                              lF      lS        lE       (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    calcSigma        <- interpolateVol volCF lF str
    return	            (yearFrac, interpPayDate, Black evDate expiry calcSigma calcFwd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        Floorlet {liborFix        = lF,  liborStart = lS,   liborEnd    = lE, liborPay = lP, 
                                  liborConvention = lC,  margin     = m,    floorStrike = str}     
                        Black    {forward         = fwd, blackSigma = sigma} 
                      = do
    (yearFrac, 
     interpPayDate, 
     evDate,
     expiry,   
     calcFwd)        <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                              lF      lS        lE       (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    calcSigma        <- interpolateVol volCF lF str
    return	            (yearFrac, interpPayDate, Black evDate expiry calcSigma calcFwd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        Caplet           {liborFix        = lF,    liborStart = lS,  liborEnd        = lE,      liborPay = lP, 
                                          liborConvention = lC,    margin     = m,   capStrike       = str}     
                        BlackNonStandard {blackSigma      = sigma, forward    = fwd, sigmaAdjustment = sigmaAd} 
                      = do
    (yearFrac, 
     interpPayDate, 
     evDate,
     expiry,   
     calcFwd)        <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                              lF      lS        lE       (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    calcSigma        <- interpolateVol volCF lF str
    calcSigmaAd      <- interpolateVol volCF lF calcFwd
    return	            (yearFrac, interpPayDate, BlackNonStandard evDate expiry calcSigma calcFwd calcSigmaAd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv              
                        Floorlet           {liborFix        = lF,    liborStart = lS,  liborEnd          = lE,      liborPay = lP, 
                                            liborConvention = lC,    margin     = m,   floorStrike       = str}     
                        BlackNonStandard   {blackSigma      = sigma, forward    = fwd, sigmaAdjustment   = sigmaAd} 
                      = do
    (yearFrac, 
     interpPayDate, 
     evDate,
     expiry,   
     calcFwd)        <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                              lF      lS        lE       (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    calcSigma        <- interpolateVol volCF lF str
    calcSigmaAd      <- interpolateVol volCF lF calcFwd
    return	            (yearFrac, interpPayDate, BlackNonStandard evDate expiry calcSigma calcFwd calcSigmaAd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        CMS              {cmsFix   = cF,  cmsDates = cDs, cmsMaturity = cMt, cmsConvention = cC}  
                        HaganRepSABRRBS2 {forward  = fwd, vAtm     = a,   beta        = b,   rho           = r, 
                                          volOfVol = vv,  xPlus    = xP,  xMinus      = xM,  nu            = n,  mu = m} 
                      = do
    (yearFrac, interpPayDate, model)          
                     <- calcCommonFieldsCMSRBS2 modParams mktData discCurve index   startDate endDate payDate (snd conv)
                                                cF        cDs     cMt       (snd cC)
    return	            (yearFrac, interpPayDate, model)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        CapletCMS              {cmsFix   = cF,  cmsDates = cDs, cmsMaturity = cMt, cmsConvention = cC}  
                        HaganRepSABRRBS2       {forward  = fwd, vAtm     = a,   beta        = b,   rho           = r, 
                                                volOfVol = vv,  xPlus    = xP,  xMinus      = xM,  nu            = n,  mu = m} 
                      = do
    (yearFrac, interpPayDate, model)          
                     <- calcCommonFieldsCMSRBS2 modParams mktData discCurve index   startDate endDate payDate (snd conv)
                                                cF        cDs     cMt       (snd cC)
    return	            (yearFrac, interpPayDate, model)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv 
                        FloorletCMS              {cmsFix   = cF,  cmsDates = cDs, cmsMaturity = cMt, cmsConvention = cC}  
                        HaganRepSABRRBS2         {forward  = fwd, vAtm     = a,   beta        = b,   rho           = r, 
                                                  volOfVol = vv,  xPlus    = xP,  xMinus      = xM,  nu            = n,  mu = m} 
                      = do
    (yearFrac, interpPayDate, model)          
                     <- calcCommonFieldsCMSRBS2 modParams mktData discCurve index   startDate endDate payDate (snd conv)
                                                cF        cDs     cMt       (snd cC)
    return	            (yearFrac, interpPayDate, model)
--------------------------------------------------------------------------------------
calcCommonFieldsLibor :: MarketData -> String    -> String 
                      -> Day        -> Day       -> Day            -> FracConvention 
                      -> Day        -> Day       -> Day            -> FracConvention 
                      -> Result (Double, Double, Day, Double, Double)
calcCommonFieldsLibor    mktData       discCurve    index            
                         startDate     endDate      payDate           conv
                         liborFix      liborStart   liborEnd          lYFracCon  
                      = do
    let dCurve        = (filter (\cv -> (curveName cv)  == discCurve)        (curves mktData))!!0
    let estCurve      = (filter (\cv -> (curveIndex cv) == (Just index))     (curves mktData))!!0
    let histFix       = (filter (\hf -> (hfIndex hf)    == index)            (historicFix mktData))!!0
    let yearFrac      = calcYearFrac startDate endDate conv
    let evDate        = refDate dCurve
    let expiry        = calcYearFrac evDate liborFix ACT365 
    interpPayDate    <- interpolateDFCurve dCurve [payDate]
    cFwd             <- calcForward estCurve liborStart liborEnd lYFracCon
    fx               <- findFix mktData index liborFix
    let fwFx          = if (liborFix < evDate) && (not $ eqtol 0.000000001 fx 0.0) then fx else cFwd
    return	            (yearFrac, (interpPayDate!!0), evDate, expiry, fwFx)        
--------------------------------------------------------------------------------------
calcCommonFieldsCMSRBS2 :: ModelParameters -> MarketData -> String    -> String
                        -> Day             -> Day        -> Day       -> FracConvention
                        -> Day             -> [Day]      -> String    -> FracConvention 
                        -> Result (Double, Double, Model)
calcCommonFieldsCMSRBS2    modParams          mktData       discCurve    index   
                           startDate          endDate       payDate      conv           
                           cmsFix             cmsDates      cmsMaturity  cmsConv 
                        = do
    let dCurve          = (filter (\cv -> (curveName cv)  == discCurve)        (curves mktData))!!0
    let estCurve        = (filter (\cv -> (curveIndex cv) == (Just index))     (curves mktData))!!0
    let histFix         = (filter (\hf -> (hfIndex hf)    == index)            (historicFix mktData))!!0
    let yearFrac        = calcYearFrac startDate endDate conv
    let evDate          = refDate dCurve
    let expry           = calcYearFrac evDate cmsFix ACT365 
    interpPayDate      <- interpolateDFCurve dCurve [payDate]
    cFwd               <- calcForwardCMS estCurve cmsDates cmsConv
    fx                 <- findFix mktData index cmsFix
    let fwFx            = if (cmsFix < evDate) && (not $ eqtol 0.000000001 fx 0.0) then fx else cFwd
    let sabr            = (filter (filterParam "SABR"  index) (parameters modParams))!!0
    let rbs2            = (filter (filterParam "RBS2"  index) (parameters modParams))!!0
    let kappa           = (filter (filterParam "KAPPA" index) (parameters modParams))!!0
    let cmsMat          = read (init cmsMaturity) :: Double
    let cA              = interpolateMatrix (vatmSABR     sabr) cmsFix cmsMat
    let cB              = interpolateMatrix (betaSABR     sabr) cmsFix cmsMat
    let cR              = interpolateMatrix (rhoSABR      sabr) cmsFix cmsMat
    let cVv             = interpolateMatrix (volOfVolSABR sabr) cmsFix cmsMat
    let cXp             = interpolateMatrix (rightStrike  rbs2) cmsFix cmsMat
    let cXm             = interpolateMatrix (leftStrike   rbs2) cmsFix cmsMat
    let cN              = interpolateMatrix (rightParams  rbs2) cmsFix cmsMat
    let cM              = interpolateMatrix (leftParams   rbs2) cmsFix cmsMat
    let cK              = interp1 (kTenors kappa) (kValues kappa) cmsMat
    return	              (yearFrac, (interpPayDate!!0), HaganRepSABRRBS2 {
                                                                           forward = fwFx, vAtm     = cA,    beta                 = cB,
                                                                           rho     = cR,   volOfVol = cVv,   xPlus                = cXp, 
                                                                           xMinus  = cXm,  nu       = cN,    mu                   = cM,
                                                                           kappa   = cK,   expiry   = expry, referenceDate        = evDate
                                                                          })        
        where 
              filterParam name index param
                  | ((show $ toConstr param) == name && 
                        paramsIndex param == index)     = True
                  | otherwise                           = False
--------------------------------------------------------------------------------------

interpolateMatrix :: ParamsData -> Day    -> Double -> Double
interpolateMatrix    params        expiry    tenor   = value
    where 
          x     = expiries params
          y     = tenors   params
          z     = matrix   params
          value = interp2 x y z (fromIntegral . toModifiedJulianDay $ expiry) tenor
--------------------------------------------------------------------------------------
findFix :: MarketData -> String -> Day -> Result Double
findFix    mktData       index     fix  = do
    let hisFix   = (filter (\hf -> (hfIndex hf) == index) (historicFix mktData))!!0
    let mHisFix  = Map.fromList $ zip (hfDates hisFix) (hfValues hisFix)
    let valueFix = Map.lookup fix mHisFix 
    get valueFix
        where 
              get :: Maybe a -> Result a
              get    Nothing  = Error ("Not found fixing for index " ++ index ++ " in date " ++ show fix)
              get    (Just x) = Ok x
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
        
        