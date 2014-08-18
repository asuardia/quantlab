module Vanilla.MarketZip   
    ( 
     getCouponFMktModInfo, getCouponVMktModInfo
    ) where
    
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
                        Libor {
                               liborFix = lF,
                               liborStart = lS, 
                               liborEnd = lE, 
                               liborPay = lP, 
                               liborConvention = lC, 
                               margin = m
                              }     
                        Forward {
                                 forward = fwd
                                } 
                      = do
    (yearFrac, 
     interpPayDate, 
     calcFwd)        <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                              lS      lE        (snd lC)
    return	            (yearFrac, interpPayDate, Forward calcFwd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        Libor {
                               liborFix = lF,
                               liborStart = lS, 
                               liborEnd = lE, 
                               liborPay = lP, 
                               liborConvention = lC, 
                               margin = m
                              }     
                        ForwardNonStandard {
                                            forward = fwd,
                                            sigmaAdjustment = sigmaAd
                                           } 
                      = do
    (yearFrac, 
     interpPayDate, 
     calcFwd)        <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                              lS      lE        (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    calcSigmaAd      <- interpolateVol volCF lF calcFwd
    return	            (yearFrac, interpPayDate, ForwardNonStandard calcFwd calcSigmaAd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        Caplet {
                               liborFix = lF,
                               liborStart = lS, 
                               liborEnd = lE, 
                               liborPay = lP, 
                               liborConvention = lC, 
                               margin = m, 
                               capStrike = str
                              }     
                        Black {
                               forward = fwd,
                               blackSigma = sigma
                              } 
                      = do
    (yearFrac, 
     interpPayDate, 
     calcFwd)        <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                              lS      lE        (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    calcSigma        <- interpolateVol volCF lF str
    return	            (yearFrac, interpPayDate, Black calcSigma calcFwd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        Floorlet {
                                  liborFix = lF,
                                  liborStart = lS, 
                                  liborEnd = lE, 
                                  liborPay = lP, 
                                  liborConvention = lC, 
                                  margin = m, 
                                  floorStrike = str
                                 }     
                        Black {
                               forward = fwd,
                               blackSigma = sigma
                              } 
                      = do
    (yearFrac, 
     interpPayDate, 
     calcFwd)        <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                              lS      lE        (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    calcSigma        <- interpolateVol volCF lF str
    return	            (yearFrac, interpPayDate, Black calcSigma calcFwd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        Caplet {
                               liborFix = lF,
                               liborStart = lS, 
                               liborEnd = lE, 
                               liborPay = lP, 
                               liborConvention = lC, 
                               margin = m, 
                               capStrike = str
                              }     
                        BlackNonStandard {
                                          blackSigma = sigma, 
                                          forward = fwd, 
                                          sigmaAdjustment = sigmaAd
                                         } 
                      = do
    (yearFrac, 
     interpPayDate, 
     calcFwd)        <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                              lS      lE        (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    calcSigma        <- interpolateVol volCF lF str
    calcSigmaAd      <- interpolateVol volCF lF calcFwd
    return	            (yearFrac, interpPayDate, BlackNonStandard calcSigma calcFwd calcSigmaAd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        Floorlet {
                                  liborFix = lF,
                                  liborStart = lS, 
                                  liborEnd = lE, 
                                  liborPay = lP, 
                                  liborConvention = lC, 
                                  margin = m, 
                                  floorStrike = str
                                 }     
                        BlackNonStandard {
                                          blackSigma = sigma, 
                                          forward = fwd, 
                                          sigmaAdjustment = sigmaAd
                                         }
                      = do
    (yearFrac, 
     interpPayDate, 
     calcFwd)        <- calcCommonFieldsLibor mktData discCurve index    startDate endDate payDate (snd conv)
                                              lS      lE        (snd lC)
    let volCF         = (filter (\cfv -> (cfIndex cfv) == index)     (capFloorVols mktData))!!0
    calcSigma        <- interpolateVol volCF lF str
    calcSigmaAd      <- interpolateVol volCF lF calcFwd
    return	            (yearFrac, interpPayDate, BlackNonStandard calcSigma calcFwd calcSigmaAd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        CMS {
                             cmsFix = cF, 
                             cmsDates = cDs, 
                             cmsConvention = cC, 
                             cmsMargin = cM
                            }  
                        HaganRepSABRRBS2 {
                                          forward = fwd,
                                          vAtm = a, 
                                          beta = b,
                                          rho = r,
                                          volOfVol = vv,
                                          xPlus = xP, 
                                          xMinus = xM,
                                          nu = n, 
                                          mu = m
                                         } 
                      = do
    (yearFrac, interpPayDate, model)          
                     <- calcCommonFieldsCMSRBS2 modParams mktData discCurve index startDate endDate payDate (snd conv)
                                                cF        cDs     (snd cC)
    return	            (yearFrac, interpPayDate, model)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        CapletCMS {
                                   cmsFix = cF, 
                                   cmsDates = cDs, 
                                   cmsConvention = cC, 
                                   cmsMargin = cM
                                  }  
                        HaganRepSABRRBS2 {
                                          forward = fwd,
                                          vAtm = a, 
                                          beta = b,
                                          rho = r,
                                          volOfVol = vv,
                                          xPlus = xP, 
                                          xMinus = xM,
                                          nu = n, 
                                          mu = m
                                         } 
                      = do
    (yearFrac, interpPayDate, model)          
                     <- calcCommonFieldsCMSRBS2 modParams mktData discCurve index startDate endDate payDate (snd conv)
                                                cF        cDs     (snd cC)
    return	            (yearFrac, interpPayDate, model)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams               mktData       discCurve     index     startDate 
                        endDate                 payDate       conv           
                        FloorletCMS {
                                     cmsFix = cF, 
                                     cmsDates = cDs, 
                                     cmsConvention = cC, 
                                     cmsMargin = cM
                                    }  
                        HaganRepSABRRBS2 {
                                          forward = fwd,
                                          vAtm = a, 
                                          beta = b,
                                          rho = r,
                                          volOfVol = vv,
                                          xPlus = xP, 
                                          xMinus = xM,
                                          nu = n, 
                                          mu = m
                                         } 
                      = do
    (yearFrac, interpPayDate, model)          
                     <- calcCommonFieldsCMSRBS2 modParams mktData discCurve index startDate endDate payDate (snd conv)
                                                cF        cDs     (snd cC)
    return	            (yearFrac, interpPayDate, model)
--------------------------------------------------------------------------------------


getCouponVMktModInfo    modParams          mktData       discCurve     index     startDate 
                        endDate            payDate       con           
                        pO
                        m 
                      = Ok (yearFrac, interpPayDate, m)
    where 
        yearFrac      = calcYearFrac startDate endDate (snd con)
        interpPayDate = 0.9    
        
--------------------------------------------------------------------------------------
calcCommonFieldsLibor :: MarketData -> String    -> String 
                      -> Day        -> Day       -> Day            -> FracConvention 
                      -> Day        -> Day       -> FracConvention 
                      -> Result (Double, Double, Double)
calcCommonFieldsLibor    mktData       discCurve    index            
                         startDate     endDate      payDate           conv
                         liborStart    liborEnd     lYFracCon  
                      = do
    let dCurve        = (filter (\cv -> (curveName cv)  == discCurve) (curves mktData))!!0
    let estCurve      = (filter (\cv -> (curveIndex cv) == (Just index))     (curves mktData))!!0
    let yearFrac      = calcYearFrac startDate endDate conv
    interpPayDate    <- interpolateDFCurve dCurve [payDate]
    calcFwd          <- calcForward estCurve liborStart liborEnd lYFracCon
    return	            (yearFrac, (interpPayDate!!0), calcFwd)        
--------------------------------------------------------------------------------------
calcCommonFieldsCMSRBS2 :: ModelParameters -> MarketData -> String    -> String
                        -> Day             -> Day        -> Day       -> FracConvention
                        -> Day             -> [Day]      -> FracConvention 
                        -> Result (Double, Double, Model)
calcCommonFieldsCMSRBS2    modParams          mktData       discCurve    index   
                           startDate          endDate       payDate      conv           
                           cmsFix             cmsDates      cmsConv 
                        = do
    let dCurve          = (filter (\cv -> (curveName cv)  == discCurve)        (curves mktData))!!0
    let estCurve        = (filter (\cv -> (curveIndex cv) == (Just index))     (curves mktData))!!0
    let yearFrac        = calcYearFrac startDate endDate conv
    interpPayDate      <- interpolateDFCurve dCurve [payDate]
    cFwd               <- calcForwardCMS estCurve cmsDates cmsConv
    let sabr            = (filter (filterParam "SABR"  index) (parameters modParams))!!0
    let rbs2            = (filter (filterParam "RBS2"  index) (parameters modParams))!!0
    let kappa           = (filter (filterParam "KAPPA" index) (parameters modParams))!!0
    let cA              = interpolateMatrix (vatmSABR     sabr) cmsFix 30.0
    let cB              = interpolateMatrix (betaSABR     sabr) cmsFix 30.0
    let cR              = interpolateMatrix (rhoSABR      sabr) cmsFix 30.0
    let cVv             = interpolateMatrix (volOfVolSABR sabr) cmsFix 30.0
    let cXp             = interpolateMatrix (rightStrike  rbs2) cmsFix 30.0
    let cXm             = interpolateMatrix (leftStrike   rbs2) cmsFix 30.0
    let cN              = interpolateMatrix (rightParams  rbs2) cmsFix 30.0
    let cM              = interpolateMatrix (leftParams   rbs2) cmsFix 30.0
    let cK              = interp1 (kTenors kappa) (kValues kappa) 30.0
    return	              (yearFrac, (interpPayDate!!0), HaganRepSABRRBS2 {
                                                                           forward = cFwd, vAtm     = cA,  beta  = cB,
                                                                           rho     = cR,   volOfVol = cVv, xPlus = cXp, 
                                                                           xMinus  = cXm,  nu       = cN,  mu    = cM,
                                                                           kappa   = cK
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

        
        
        