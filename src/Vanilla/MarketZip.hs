module Vanilla.MarketZip   
    ( 
     getCouponMktModInfo
    ) where
    
import qualified Data.Map as Map
import Data.Data
import Data.Typeable
import Data.Time.Calendar
import Utils.MyJSON
import Vanilla.Types
import Vanilla.ModelParameters
import Vanilla.Models
import Vanilla.PayOffs
import Vanilla.Curves
import Vanilla.Volatility
import Market.FinantialConventions
import Market.YearFractions
import Market.MarketData
import Math.Interpolation

getCouponMktModInfo :: ModelParameters -> MarketData -> Coupon 
                     -> Result Coupon
getCouponMktModInfo    modParams          mktData       Fixed {cpStartDate  = sd, cpEndDate = ed, cpPayDate    = pd, cpYearFrac  = yearFrac, cpRemainingCapital = rc, 
                                                               cpConvention = cn, fxRate    = rt, fxDiscFactor = discfactor, cpDiscCurve = dc}
		             = do 
    let dCurve       = (filter (\cv -> (curveName cv) == dc) (curves mktData))!!0
    let yearFrac'    = calcYearFrac sd ed (snd cn)
    discfactor'     <- interpolateDFCurve dCurve [pd]
    return	         Fixed {cpStartDate  = sd, cpEndDate = ed, cpPayDate    = pd, cpYearFrac  = yearFrac', cpRemainingCapital = rc, 
                            cpConvention = cn, fxRate    = rt, fxDiscFactor = discfactor'!!0, cpDiscCurve = dc}    
--------------------------------------------------------------------------------------
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate, cpYearFrac  = yearFrac, 
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0,    cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = Libor   {liborFix = lF,  liborStart = lS, liborEnd = lE, liborPay = lP, liborConvention = lC, margin = m},
                                 varModel           = Forward {referenceDate = evDate, forward  = fwd}} 
                     = do
    (yearFrac', 
     num0',
     evDate',
     expir',
     t2Pay',
     fwd')          <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                             lF      lS        lE       (snd lC)
    return	           Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate,  cpYearFrac  = yearFrac', 
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0', cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = Libor   {liborFix = lF,  liborStart = lS, liborEnd = lE, liborPay = lP, liborConvention = lC, margin = m},
                                 varModel           = Forward {referenceDate = evDate', forward  = fwd'}} 
--------------------------------------------------------------------------------------
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate, cpYearFrac  = yearFrac, 
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0,    cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = Libor   {liborFix = lF,  liborStart = lS, liborEnd = lE, liborPay = lP, liborConvention = lC, margin = m},
                                 varModel           = ForwardNonStandard {referenceDate = evDate, expiry = expir, time2Pay = t2Pay, forward  = fwd, sigmaAdjustment = sigmaAd}} 
                      = do
    (yearFrac', 
     num0',
     evDate',
     expir',   
     t2Pay',
     fwd')           <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                             lF      lS        lE       (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    sigmaAd'         <- interpolateVol volCF lF fwd'
    return	            Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate,  cpYearFrac  = yearFrac', 
                                  cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0', cpDiscCurve = discCurve, cpIndex = index,
                                  varPayOff          = Libor   {liborFix = lF,  liborStart = lS, liborEnd = lE, liborPay = lP, liborConvention = lC, margin = m},
                                  varModel           = ForwardNonStandard {referenceDate = evDate', expiry = expir', time2Pay = t2Pay', forward  = fwd', sigmaAdjustment = sigmaAd'}} 
--------------------------------------------------------------------------------------
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate, cpYearFrac  = yearFrac, 
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0,    cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = Caplet {liborFix = lF,  liborStart = lS, liborEnd = lE, liborPay = lP, liborConvention = lC, margin = m, capStrike = str},   
                                 varModel           = Black {referenceDate = evDate, expiry = expir, forward = fwd, blackSigma = sigma}} 
                      = do
    (yearFrac', 
     num0',
     evDate',
     expir',   
     t2Pay',
     fwd')           <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                             lF      lS        lE       (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    sigma'           <- interpolateVol volCF lF str
    return	            Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate,  cpYearFrac  = yearFrac', 
                                  cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0', cpDiscCurve = discCurve, cpIndex = index,
                                  varPayOff          = Caplet {liborFix = lF,  liborStart = lS, liborEnd = lE, liborPay = lP, liborConvention = lC, margin = m, capStrike = str},    
                                  varModel           = Black {referenceDate = evDate', expiry = expir', forward = fwd', blackSigma = sigma'}} 
--------------------------------------------------------------------------------------
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate, cpYearFrac  = yearFrac, 
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0,    cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = Floorlet {liborFix = lF,  liborStart = lS, liborEnd = lE, liborPay = lP, liborConvention = lC, margin = m, floorStrike = str},   
                                 varModel           = Black {referenceDate = evDate, expiry = expir, forward = fwd, blackSigma = sigma}} 
                      = do
    (yearFrac', 
     num0',
     evDate',
     expir',   
     t2Pay',
     fwd')           <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                             lF      lS        lE       (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    sigma'           <- interpolateVol volCF lF str
    return	            Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate,  cpYearFrac  = yearFrac', 
                                  cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0', cpDiscCurve = discCurve, cpIndex = index,
                                  varPayOff          = Floorlet {liborFix = lF,  liborStart = lS, liborEnd = lE, liborPay = lP, liborConvention = lC, margin = m, floorStrike = str},    
                                  varModel           = Black {referenceDate = evDate', expiry = expir', forward = fwd', blackSigma = sigma'}} 
--------------------------------------------------------------------------------------
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate, cpYearFrac  = yearFrac, 
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0,    cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = Caplet {liborFix = lF,  liborStart = lS, liborEnd = lE, liborPay = lP, liborConvention = lC, margin = m, capStrike = str},   
                                 varModel           = BlackNonStandard {referenceDate = evDate, expiry = expir, time2Pay = t2Pay, forward = fwd, blackSigma = sigma, sigmaAdjustment   = sigmaAd}} 
                      = do
    (yearFrac', 
     num0',
     evDate',
     expir',   
     t2Pay',
     fwd')           <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                             lF      lS        lE       (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    sigma'           <- interpolateVol volCF lF str
    sigmaAd'         <- interpolateVol volCF lF fwd'
    return	            Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate,  cpYearFrac  = yearFrac', 
                                  cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0', cpDiscCurve = discCurve, cpIndex = index,
                                  varPayOff          = Caplet {liborFix = lF,  liborStart = lS, liborEnd = lE, liborPay = lP, liborConvention = lC, margin = m, capStrike = str},    
                                  varModel           = BlackNonStandard {referenceDate = evDate', expiry = expir', time2Pay = t2Pay', forward = fwd', blackSigma = sigma', sigmaAdjustment   = sigmaAd'}} 
--------------------------------------------------------------------------------------
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate, cpYearFrac  = yearFrac, 
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0,    cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = Floorlet {liborFix = lF,  liborStart = lS, liborEnd = lE, liborPay = lP, liborConvention = lC, margin = m, floorStrike = str},   
                                 varModel           = BlackNonStandard {referenceDate = evDate, expiry = expir, time2Pay = t2Pay, forward = fwd, blackSigma = sigma, sigmaAdjustment   = sigmaAd}} 
                      = do
    (yearFrac', 
     num0',
     evDate',
     expir',   
     t2Pay',
     fwd')           <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                             lF      lS        lE       (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    sigma'           <- interpolateVol volCF lF str
    sigmaAd'         <- interpolateVol volCF lF fwd'
    return	            Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate,  cpYearFrac  = yearFrac', 
                                  cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0', cpDiscCurve = discCurve, cpIndex = index,
                                  varPayOff          = Floorlet {liborFix = lF,  liborStart = lS, liborEnd = lE, liborPay = lP, liborConvention = lC, margin = m, floorStrike = str},    
                                  varModel           = BlackNonStandard {referenceDate = evDate', expiry = expir', time2Pay = t2Pay', forward = fwd', blackSigma = sigma', sigmaAdjustment   = sigmaAd'}} 
--------------------------------------------------------------------------------------
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate, cpYearFrac  = yearFrac, 
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0,    cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = CMS {cmsFix   = cF,  cmsDates = cDs, cmsMaturity = cMt, cmsConvention = cC, cmsMargin = cM} , 
                                 varModel           = HaganRepSABRRBS2 {referenceDate = evDate, expiry = expir}}
                     = do
    (yearFrac', num0', model')          
                    <- calcCommonFieldsCMSRBS2 modParams mktData discCurve index startDate endDate payDate (snd conv)
                                                cF        cDs     cMt       (snd cC)
    return	           Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate,  cpYearFrac  = yearFrac', 
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0', cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = CMS {cmsFix   = cF,  cmsDates = cDs, cmsMaturity = cMt, cmsConvention = cC, cmsMargin = cM} , 
                                 varModel           = model'} 
--------------------------------------------------------------------------------------
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate, cpYearFrac  = yearFrac, 
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0,    cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = CapletCMS {cmsFix   = cF,  cmsDates = cDs, cmsMaturity = cMt, cmsConvention = cC, cmsMargin = cM, capStrike = cStr} , 
                                 varModel           = HaganRepSABRRBS2 {referenceDate = evDate, expiry = expir}}
                     = do
    (yearFrac', num0', model')          
                    <- calcCommonFieldsCMSRBS2 modParams mktData discCurve index startDate endDate payDate (snd conv)
                                                cF        cDs     cMt       (snd cC)
    return	           Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate,  cpYearFrac  = yearFrac', 
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0', cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = CapletCMS {cmsFix   = cF,  cmsDates = cDs, cmsMaturity = cMt, cmsConvention = cC, cmsMargin = cM, capStrike = cStr} , 
                                 varModel           = model'} 
--------------------------------------------------------------------------------------
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate, cpYearFrac  = yearFrac, 
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0,    cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = FloorletCMS {cmsFix   = cF,  cmsDates = cDs, cmsMaturity = cMt, cmsConvention = cC, cmsMargin = cM, floorStrike = cStr} , 
                                 varModel           = HaganRepSABRRBS2 {referenceDate = evDate, expiry = expir}}
                     = do
    (yearFrac', num0', model')          
                    <- calcCommonFieldsCMSRBS2 modParams mktData discCurve index startDate endDate payDate (snd conv)
                                                cF        cDs     cMt       (snd cC)
    return	           Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate,  cpYearFrac  = yearFrac', 
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0', cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = FloorletCMS {cmsFix   = cF,  cmsDates = cDs, cmsMaturity = cMt, cmsConvention = cC, cmsMargin = cM, floorStrike = cStr} , 
                                 varModel           = model'} 
--------------------------------------------------------------------------------------
calcCommonFieldsLibor :: MarketData -> String    -> String 
                      -> Day        -> Day       -> Day            -> FracConvention 
                      -> Day        -> Day       -> Day            -> FracConvention 
                      -> Result (Double, Double, Day, Double, Double, Double)
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
    let time2Pay      = calcYearFrac evDate payDate ACT365 
    interpPayDate    <- interpolateDFCurve dCurve [payDate]
    cFwd             <- calcForward estCurve liborStart liborEnd lYFracCon
    fx               <- findFix mktData index liborFix
    let fwFx          = if (liborFix < evDate) && (not $ eqtol 0.000000001 fx 0.0) then fx else cFwd
    return	            (yearFrac, (interpPayDate!!0), evDate, expiry, time2Pay, fwFx)        
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
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
        
        