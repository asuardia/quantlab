module Vanilla.MarketZip   
    ( 
     getCouponFMktModInfo, getCouponVMktModInfo
    ) where
    
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
getCouponVMktModInfo :: ModelParameters -> MarketData -> String     -> String -> Day 
                     -> Day             -> Day        -> Convention 
                     -> PayOff 
                     -> Model 
                     -> Result (Double, Double, Model)
getCouponVMktModInfo    modParams          mktData       discCurve     index     startDate 
                        endDate            payDate       con           
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
    let dCurve        = (filter (\cv -> (curveName cv)  == discCurve) (curves mktData))!!0
    let estCurve      = (filter (\cv -> (curveIndex cv) == (Just index))     (curves mktData))!!0
    let yearFrac      = calcYearFrac startDate endDate (snd lC)
    interpPayDate    <- interpolateDFCurve dCurve [startDate]
    calcFwd          <- calcForward estCurve lS lE (snd lC)
    return	            (yearFrac, (interpPayDate!!0), Forward calcFwd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams          mktData       discCurve     index     startDate 
                        endDate            payDate       con           
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
    let dCurve        = (filter (\cv -> (curveName cv)  == discCurve) (curves mktData))!!0
    let estCurve      = (filter (\cv -> (curveIndex cv) == (Just index))     (curves mktData))!!0
    let volCF         = (filter (\cfv -> (cfvIndex cfv) == index)     (capFloorVols mktData))!!0
    let yearFrac      = calcYearFrac startDate endDate (snd lC)
    interpPayDate    <- interpolateDFCurve dCurve [startDate]
    calcFwd          <- calcForward estCurve lS lE (snd lC)
    calcSigmaAd      <- interpolateVol volCF lF calcFwd
    return	            (yearFrac, (interpPayDate!!0), ForwardNonStandard calcFwd calcSigmaAd)
--------------------------------------------------------------------------------------
getCouponVMktModInfo    modParams          mktData       discCurve     index     startDate 
                        endDate            payDate       con           
                        pO
                        m 
                      = Ok (yearFrac, interpPayDate, m)
    where 
        yearFrac      = calcYearFrac startDate endDate (snd con)
        interpPayDate = 0.9    
        
        
        

        
        
        