module Vanilla.MarketZip2   
    ( 
     getCouponMktModInfo2, getSwptnMktModInfo, findFix 
    ) where
    
import qualified Data.Map as Map
import Data.Data
import Data.Typeable
import Data.Time.Calendar
import Utils.MyJSON
import Utils.MyUtils
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

getCouponMktModInfo2 :: ModelParameters -> MarketData -> Coupon 
                     -> Result Coupon
-------------------------------------------------------------------------- 

getCouponMktModInfo2    modParams         mktData       
                       Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                 cpPayDate    = payDate,   cpYearFrac         = yearFrac,  
                                 cpEstCurve   = estCurve,  cpRemainingCapital = remCap,    
                                 cpConvention = conv,      varNum0            = num0,    
                                 cpDiscCurve  = discCurve, cpIndex            = index,
                                 varPayOff = CMS {cmsFix      = cF,  cmsDates      = cDs, 
                                                  cmsMaturity = cMt, cmsConvention = cC, 
                                                  cmsMargin   = cM} , 
                                 varModel = HaganRepSABRRBS2 {referenceDate = evDate, expiry = expir}}
                     = do
    (yearFrac', num0', model')          
                    <- calcCommonFieldsCMSRBS2 modParams mktData   discCurve 
                                               index     startDate endDate 
                                               payDate   (snd conv) 
                                                cF        cDs     cMt       (snd cC)
    return	           Variable{ cpStartDate  = startDate, cpEndDate          = endDate, 
                                 cpPayDate    = payDate,   cpYearFrac         = yearFrac', 
                                 cpEstCurve   = estCurve,  cpRemainingCapital = remCap,    
                                 cpConvention = conv,      varNum0            = num0',   
                                 cpDiscCurve  = discCurve, cpIndex            = index,
                                 varPayOff          = CMS {cmsFix      = cF,  cmsDates      = cDs, 
                                                           cmsMaturity = cMt, cmsConvention = cC, 
                                                           cmsMargin   = cM} , 
                                 varModel = model'} 
-------------------------------------------------------------------------- 
getCouponMktModInfo2    modParams         mktData       
                       Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate, cpYearFrac  = yearFrac,  cpEstCurve = estCurve,
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0,    cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = CapletCMS {cmsFix   = cF,  cmsDates = cDs, cmsMaturity = cMt, cmsConvention = cC, cmsMargin = cM, capStrike = cStr} , 
                                 varModel           = HaganRepSABRRBS2 {referenceDate = evDate, expiry = expir}}
                     = do
    (yearFrac', num0', model')          
                    <- calcCommonFieldsCMSRBS2 modParams mktData discCurve index startDate endDate payDate (snd conv)
                                                cF        cDs     cMt       (snd cC)
    return	           Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate, cpYearFrac  = yearFrac', cpEstCurve = estCurve,
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0',   cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = CapletCMS {cmsFix   = cF,  cmsDates = cDs, cmsMaturity = cMt, cmsConvention = cC, cmsMargin = cM, capStrike = cStr} , 
                                 varModel           = model'} 
-------------------------------------------------------------------------- 
getCouponMktModInfo2    modParams         mktData       
                       Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate, cpYearFrac  = yearFrac,  cpEstCurve = estCurve,
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0,    cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = FloorletCMS {cmsFix   = cF,  cmsDates = cDs, cmsMaturity = cMt, cmsConvention = cC, cmsMargin = cM, floorStrike = cStr} , 
                                 varModel           = HaganRepSABRRBS2 {referenceDate = evDate, expiry = expir}}
                     = do
    (yearFrac', num0', model')          
                    <- calcCommonFieldsCMSRBS2 modParams mktData discCurve index startDate endDate payDate (snd conv)
                                                cF        cDs     cMt       (snd cC)
    return	           Variable{ cpStartDate        = startDate, cpEndDate    = endDate, cpPayDate = payDate, cpYearFrac  = yearFrac', cpEstCurve = estCurve,
                                 cpRemainingCapital = remCap,    cpConvention = conv,    varNum0   = num0',   cpDiscCurve = discCurve, cpIndex = index,
                                 varPayOff          = FloorletCMS {cmsFix   = cF,  cmsDates = cDs, cmsMaturity = cMt, cmsConvention = cC, cmsMargin = cM, floorStrike = cStr} , 
                                 varModel           = model'} 
                                 
-------------------------------------------------------------------------- 

getSwptnMktModInfo :: ModelParameters -> MarketData -> Product 
                   -> Result Product

getSwptnMktModInfo    modParams         mktData       
                      Swaption {swptnSwap = s@(Swap {swLeg1 = fl@(FixedLeg {}), 
                                                     swLeg2 = fll@(VariableLeg {})}),
                                swptnExerDate = ed,   swptnTypePO = Delivery,
                                swptnStrike   = str, swptnNum0   = n0, 
                                swptnModel    = bl@(Black {}),
                                swptnCurr     = curr, swptnCap = rc}
                    = do
    let dCurve      = (filter (\cv -> (curveName cv) == discCurve fll) (curves mktData))!!0
    let eCurve      = (filter (\cv -> (curveName cv) == estCurve  fll) (curves mktData))!!0
    let histFix     = (filter (\hf -> (hfIndex hf)   == legIndex  fll) (historicFix mktData))!!0
    let evDate      = refDate dCurve
    let expry       = calcYearFrac evDate ed ACT365
    (fwFx, ann)    <- forwardOrFix evDate ed eCurve
    let volSw       = filter (\swv -> (swCurr swv) == curr) (swaptionVols mktData)!!0
    sigma          <- interpolateVolSw volSw ed str (5)
    let model       = Black evDate expry sigma fwFx
    return	          Swaption {swptnSwap   = s,        swptnExerDate = ed,
                                swptnTypePO = Delivery, swptnStrike   = str,
                                swptnNum0   = ann     , swptnModel    = model,
                                swptnCurr   = curr    , swptnCap   = rc}
        where 
              forwardOrFix :: Day -> Day -> RateCurve -> Result (Double, Double)
              forwardOrFix    eD     fD     eCurve
                  | fD <= eD  = do 
                                fix <- findFix mktData (legIndex fll) ed 
                                --let cmsDates = (cpStartDate $ head cpns):(fmap cpEndDate cpns)                                                       
                                (fwd, ann) <- calcForwardCMS eCurve cmsDates (snd $ cpConvention $ head cpns)
                                return (if (not $ eqtol 0.000000001 fix 0.0) 
                                        then (fix, ann) else (fwd, ann))
                  | otherwise = calcForwardCMS eCurve cmsDates (snd $ cpConvention $ head cpns)
                        where 
                              cpns = coupons fl 
                              cmsDates = (cpStartDate $ head cpns):(fmap cpEndDate cpns)          
-------------------------------------------------------------------------- 

calcCommonFieldsCMSRBS2 :: ModelParameters -> MarketData -> String    -> String
                        -> Day             -> Day        -> Day       -> FracConvention
                        -> Day             -> [Day]      -> String    -> FracConvention 
                        -> Result (Double, Double, Model)
calcCommonFieldsCMSRBS2    modParams          mktData       discCurve    index   
                           startDate          endDate       payDate      conv           
                           cmsFix             cmsDates      cmsMaturity  cmsConv 
                        = do
    let dCurve          = (filter (\cv -> (curveName cv)  == discCurve)    (curves mktData))!!0
    let estCurve        = (filter (\cv -> (curveIndex cv) == (Just index)) (curves mktData))!!0
    let histFix         = (filter (\hf -> (hfIndex hf)    == index)        (historicFix mktData))!!0
    let yearFrac        = calcYearFrac startDate endDate conv
    let evDate          = refDate dCurve
    let expry           = calcYearFrac evDate cmsFix ACT365 
    interpPayDate      <- interpolateDFCurve dCurve [payDate]
    fwFx               <- forwardOrFix evDate cmsFix estCurve
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
    return	              (yearFrac, 
                           (interpPayDate!!0), 
                           HaganRepSABRRBS2 {
                                               forward = fwFx, vAtm     = cA,    beta                 = cB,
                                               rho     = cR,   volOfVol = cVv,   xPlus                = cXp, 
                                               xMinus  = cXm,  nu       = cN,    mu                   = cM,
                                               kappa   = cK,   expiry   = expry, referenceDate        = evDate
                                              })        
        where 
              filterParam name index param
                  | ((show $ toConstr param) == name && 
                      paramsIndex param      == index)  = True
                  | otherwise                           = False
              forwardOrFix :: Day -> Day -> RateCurve -> Result Double
              forwardOrFix    eD     fD     estCurve
                  | fD <= eD  = do 
                                fix      <- findFix mktData index cmsFix 
                                (fwd, _) <- calcForwardCMS estCurve cmsDates cmsConv
                                return (if (not $ eqtol 0.000000001 fix 0.0) then fix else fwd)
                  | otherwise = do
                                (fwd, _) <- calcForwardCMS estCurve cmsDates cmsConv 
                                return fwd
-------------------------------------------------------------------------- 

interpolateMatrix :: ParamsData -> Day    -> Double -> Double
interpolateMatrix    params        expiry    tenor   = value
    where 
          x     = expiries params
          y     = tenors   params
          z     = matrix   params
          value = interp2 x y z (fromIntegral . toModifiedJulianDay $ expiry) tenor
-------------------------------------------------------------------------- 
findFix :: MarketData -> String -> Day -> Result Double
findFix    mktData       index     fix  = do
    let hisFix   = (filter (\hf -> (hfIndex hf) == index) (historicFix mktData))!!0
    let mHisFix  = Map.fromList $ zip (hfDates hisFix) (hfValues hisFix)
    let valueFix = Map.lookup fix mHisFix 
    get valueFix
        where 
              get :: Maybe a -> Result a
              get    Nothing  = Error ("Not found fixing for index " 
                                       ++ index ++ " in date " ++ show fix)
              get    (Just x) = Ok x