module Vanilla.MarketZip   
    ( 
     getCouponMktModInfo
    ) where
    
import qualified Data.Map as Map
import Data.Data
import Data.Typeable
import Data.Time.Calendar
import Utils.MyJSON
import Utils.MyUtils
import Vanilla.MarketZip2
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
  
-------------------------------------------------------------------------- 

getCouponMktModInfo :: ModelParameters -> MarketData -> Coupon 
                     -> Result Coupon
getCouponMktModInfo    modParams          mktData       
                       Fixed {cpStartDate  = sd,         cpEndDate          = ed,  
                              cpYearFrac   = yearFrac,   cpRemainingCapital = rc, 
                              cpConvention = cn,         fxRate             = rt, 
                              fxDiscFactor = discfactor, cpDiscCurve = dc,
                              cpPayDate    = pd}
		             = do 
    let dCurve       = (filter (\cv -> (curveName cv) == dc) (curves mktData))!!0
    let yearFrac'    = calcYearFrac sd ed (snd cn)
    discfactor'     <- interpolateDFCurve dCurve [pd]
    return	           Fixed {cpStartDate  = sd,             cpEndDate          = ed, 
                              cpYearFrac   = yearFrac',      cpRemainingCapital = rc, 
                              cpConvention = cn,             fxRate             = rt, 
                              fxDiscFactor = discfactor'!!0, cpDiscCurve        = dc,
                              cpPayDate    = pd}    
-------------------------------------------------------------------------- 
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                 cpPayDate    = payDate,   cpYearFrac         = yearFrac,  
                                 cpEstCurve   = estCurve,  cpRemainingCapital = remCap,    
                                 cpConvention = conv,      varNum0            = num0,    
                                 cpDiscCurve  = discCurve, cpIndex            = index,
                                 varPayOff = Libor{liborFix = lF,        liborStart = lS, 
                                                   liborEnd = lE,        liborPay   = lP, 
                                                   liborConvention = lC, margin     = m},
                                 varModel  = Forward {referenceDate = evDate, forward  = fwd}} 
                     = do
    (yearFrac', 
     num0',
     evDate',
     expir',
     t2Pay',
     fwd')          <- calcCommonFieldsLibor mktData discCurve index startDate endDate payDate (snd conv)
                                             lF      lS        lE    (snd lC)
    return	           Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                 cpPayDate    = payDate,   cpYearFrac         = yearFrac', 
                                 cpEstCurve   = estCurve,  cpRemainingCapital = remCap,    
                                 cpConvention = conv,      varNum0            = num0',   
                                 cpDiscCurve  = discCurve, cpIndex            = index,
                                 varPayOff = Libor   {liborFix        = lF, liborStart = lS, 
                                                      liborEnd        = lE, liborPay   = lP, 
                                                      liborConvention = lC, margin     = m},
                                 varModel  = Forward {referenceDate = evDate', forward  = fwd'}} 
-------------------------------------------------------------------------- 
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                 cpPayDate    = payDate,   cpYearFrac         = yearFrac,  
                                 cpEstCurve   = estCurve,  cpRemainingCapital = remCap,    
                                 cpConvention = conv,      varNum0            = num0,    
                                 cpDiscCurve  = discCurve, cpIndex            = index,
                                 varPayOff = Libor {liborFix = lF,  liborStart = lS, 
                                                    liborEnd = lE, liborPay = lP, 
                                                    liborConvention = lC, margin = m},
                                 varModel  = ForwardNonStandard {referenceDate   = evDate, expiry  = expir,
                                                                 time2Pay        = t2Pay,  forward = fwd, 
                                                                 sigmaAdjustment = sigmaAd}} 
                      = do
    (yearFrac', 
     num0',
     evDate',
     expir',   
     t2Pay',
     fwd')           <- calcCommonFieldsLibor mktData discCurve index startDate endDate payDate (snd conv)
                                              lF      lS        lE    (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index) (capFloorVols mktData))!!0
    sigmaAd'         <- interpolateVol volCF lF fwd'
    return	            Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                  cpPayDate    = payDate,   cpYearFrac         = yearFrac', 
                                  cpEstCurve   = estCurve,  cpRemainingCapital = remCap,    
                                  cpConvention = conv,      varNum0            = num0',   
                                  cpDiscCurve  = discCurve, cpIndex            = index,
                                  varPayOff = Libor {liborFix        = lF, liborStart = lS, 
                                                     liborEnd        = lE, liborPay   = lP, 
                                                     liborConvention = lC, margin     = m},
                                  varModel = ForwardNonStandard {referenceDate   = evDate', expiry   = expir', 
                                                                 time2Pay        = t2Pay',  forward  = fwd', 
                                                                 sigmaAdjustment = sigmaAd'}} 
-------------------------------------------------------------------------- 
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                 cpPayDate    = payDate,   cpYearFrac         = yearFrac,  
                                 cpEstCurve   = estCurve,  cpRemainingCapital = remCap,   
                                 cpConvention = conv,      varNum0            = num0,    
                                 cpDiscCurve  = discCurve, cpIndex            = index,
                                 varPayOff = Caplet {liborFix        = lF, liborStart = lS, 
                                                     liborEnd        = lE, liborPay   = lP, 
                                                     liborConvention = lC, margin     = m, 
                                                     capStrike       = str},   
                                 varModel = Black {referenceDate = evDate, expiry     = expir, 
                                                   forward       = fwd,    blackSigma = sigma}} 
                      = do
    (yearFrac', 
     num0',
     evDate',
     expir',   
     t2Pay',
     fwd')           <- calcCommonFieldsLibor mktData discCurve index startDate endDate payDate (snd conv)
                                              lF      lS        lE    (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index) (capFloorVols mktData))!!0
    sigma'           <- interpolateVol volCF lF str
    return	            Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                  cpPayDate    = payDate,   cpYearFrac         = yearFrac',  
                                  cpEstCurve   = estCurve,  cpRemainingCapital = remCap,    
                                  cpConvention = conv,      varNum0            = num0',   
                                  cpDiscCurve  = discCurve, cpIndex            = index,
                                  varPayOff = Caplet {liborFix        = lF, liborStart = lS, 
                                                      liborEnd        = lE, liborPay   = lP, 
                                                      liborConvention = lC, margin     = m, 
                                                      capStrike       = str},    
                                  varModel = Black {referenceDate = evDate', expiry     = expir', 
                                                    forward       = fwd',    blackSigma = sigma'}} 
-------------------------------------------------------------------------- 
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                 cpPayDate    = payDate,   cpYearFrac         = yearFrac,  
                                 cpEstCurve   = estCurve,  cpRemainingCapital = remCap,   
                                 cpConvention = conv,      varNum0            = num0,    
                                 cpDiscCurve  = discCurve, cpIndex            = index,
                                 varPayOff = Floorlet {liborFix        = lF, liborStart = lS, 
                                                       liborEnd        = lE, liborPay   = lP, 
                                                       liborConvention = lC, margin     = m, 
                                                       floorStrike     = str},   
                                 varModel = Black {referenceDate = evDate, expiry     = expir, 
                                                   forward       = fwd,    blackSigma = sigma}} 
                      = do
    (yearFrac', 
     num0',
     evDate',
     expir',   
     t2Pay',
     fwd')           <- calcCommonFieldsLibor mktData discCurve index startDate endDate payDate (snd conv)
                                              lF      lS        lE    (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index) (capFloorVols mktData))!!0
    sigma'           <- interpolateVol volCF lF str
    return	            Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                  cpPayDate    = payDate,   cpYearFrac         = yearFrac',  
                                  cpEstCurve   = estCurve,  cpRemainingCapital = remCap,    
                                  cpConvention = conv,      varNum0            = num0',   
                                  cpDiscCurve  = discCurve, cpIndex            = index,
                                  varPayOff = Floorlet {liborFix        = lF, liborStart = lS, 
                                                        liborEnd        = lE, liborPay   = lP, 
                                                        liborConvention = lC, margin     = m, 
                                                        floorStrike     = str},    
                                  varModel = Black {referenceDate = evDate', expiry     = expir', 
                                                    forward       = fwd',    blackSigma = sigma'}} 
-------------------------------------------------------------------------- 
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                 cpPayDate    = payDate,   cpYearFrac         = yearFrac,  
                                 cpEstCurve   = estCurve,  cpRemainingCapital = remCap,    
                                 cpConvention = conv,      varNum0            = num0,    
                                 cpDiscCurve  = discCurve, cpIndex            = index,
                                 varPayOff = Caplet {liborFix        = lF, liborStart = lS, 
                                                     liborEnd        = lE, liborPay   = lP, 
                                                     liborConvention = lC, margin     = m, 
                                                     capStrike       = str},   
                                 varModel = BlackNonStandard {referenceDate = evDate, expiry          = expir, 
                                                              time2Pay      = t2Pay,  forward         = fwd, 
                                                              blackSigma    = sigma,  sigmaAdjustment = sigmaAd}} 
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
    return	            Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                  cpPayDate    = payDate,   cpYearFrac         = yearFrac', 
                                  cpEstCurve   = estCurve,  cpRemainingCapital = remCap,    
                                  cpConvention = conv,      varNum0            = num0',   
                                  cpDiscCurve  = discCurve, cpIndex            = index,
                                  varPayOff = Caplet {liborFix        = lF, liborStart = lS, 
                                                      liborEnd        = lE, liborPay   = lP, 
                                                      liborConvention = lC, margin     = m, 
                                                      capStrike       = str},    
                                  varModel  = BlackNonStandard {referenceDate = evDate', expiry            = expir', 
                                                                time2Pay      = t2Pay',  forward           = fwd', 
                                                                blackSigma    = sigma',  sigmaAdjustment   = sigmaAd'}} 
-------------------------------------------------------------------------- 
getCouponMktModInfo    modParams         mktData       
                       Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                 cpPayDate    = payDate,   cpYearFrac         = yearFrac,  
                                 cpEstCurve   = estCurve,  cpRemainingCapital = remCap,    
                                 cpConvention = conv,      varNum0            = num0,    
                                 cpDiscCurve  = discCurve, cpIndex            = index,
                                 varPayOff = Floorlet {liborFix        = lF, liborStart = lS, 
                                                       liborEnd        = lE, liborPay   = lP, 
                                                       liborConvention = lC, margin     = m, 
                                                       floorStrike     = str},   
                                 varModel = BlackNonStandard {referenceDate = evDate, expiry          = expir, 
                                                              time2Pay      = t2Pay,  forward         = fwd, 
                                                              blackSigma    = sigma,  sigmaAdjustment = sigmaAd}} 
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
    return	            Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                  cpPayDate    = payDate,   cpYearFrac         = yearFrac', 
                                  cpEstCurve   = estCurve,  cpRemainingCapital = remCap,    
                                  cpConvention = conv,      varNum0            = num0',   
                                  cpDiscCurve  = discCurve, cpIndex            = index,
                                  varPayOff = Floorlet {liborFix        = lF, liborStart = lS, 
                                                        liborEnd        = lE, liborPay   = lP, 
                                                        liborConvention = lC, margin     = m, 
                                                        floorStrike     = str},    
                                  varModel  = BlackNonStandard {referenceDate = evDate', expiry            = expir', 
                                                                time2Pay      = t2Pay',  forward           = fwd', 
                                                                blackSigma    = sigma',  sigmaAdjustment   = sigmaAd'}} 
--------------------------------------------------------------------------
getCouponMktModInfo    modParams  mktData coupon
                     = getCouponMktModInfo2 modParams  mktData coupon
--------------------------------------------------------------------------
calcCommonFieldsLibor :: MarketData -> String    -> String 
                      -> Day        -> Day       -> Day            -> FracConvention 
                      -> Day        -> Day       -> Day            -> FracConvention 
                      -> Result (Double, Double, Day, Double, Double, Double)
calcCommonFieldsLibor    mktData       discCurve    index            
                         startDate     endDate      payDate           conv
                         liborFix      liborStart   liborEnd          lYFracCon  
                      = do
    let dCurve        = (filter (\cv -> (curveName cv)  == discCurve)    (curves mktData))!!0
    let estCurve      = (filter (\cv -> (curveIndex cv) == (Just index)) (curves mktData))!!0
    let histFix       = (filter (\hf -> (hfIndex hf)    == index)        (historicFix mktData))!!0
    let yearFrac      = calcYearFrac startDate endDate conv
    let evDate        = refDate dCurve
    let expiry        = calcYearFrac evDate liborFix ACT365 
    let time2Pay      = calcYearFrac evDate payDate ACT365 
    interpPayDate    <- interpolateDFCurve dCurve [payDate]
    fwFx             <- forwardOrFix evDate liborFix estCurve
    return	            (yearFrac, (interpPayDate!!0), evDate, expiry, time2Pay, fwFx)    
    where 
          forwardOrFix :: Day -> Day -> RateCurve -> Result Double
          forwardOrFix    eD     fD     estCurve
              | fD <= eD  = do 
                            fix <- findFix mktData index liborFix 
                            fwd <- calcForward estCurve liborStart liborEnd lYFracCon
                            return (if (not $ eqtol 0.000000001 fix 0.0) then fix else fwd)
              | otherwise = calcForward estCurve liborStart liborEnd lYFracCon
--------------------------------------------------------------------------  
 
 
 

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
        
        