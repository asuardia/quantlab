module Vanilla.MarketZip   
    ( 
     getCouponFMktModInfo, getCouponVMktModInfo
    ) where
    
import Data.Time.Calendar
import Utils.MyJSON
import Vanilla.ModelParameters
import Vanilla.Models
import Vanilla.PayOffs
import Market.FinantialConventions
import Market.YearFractions
import Market.MarketData

getCouponFMktModInfo :: ModelParameters -> MarketData -> String -> Day             
                     -> Day             -> Day        -> Convention 
                     -> Result (Double, Double)
getCouponFMktModInfo    modParams mktData discCurve startDate 
                        endDate   payDate conv 
                      = Ok (yearFrac, interpPayDate)
    where 
          dCurve        = filter (\cv -> (curveName cv) == discCurve) (curves mktData)
          yearFrac      = calcYearFrac startDate endDate (snd conv)
          interpPayDate = 0.9 --interpolateCurve dCurve startDate
          

getCouponVMktModInfo :: ModelParameters -> MarketData -> String ->     String -> Day 
                     -> Day             -> Day        -> Convention -> PayOff -> Model 
                     -> Result (Double, Double, Model)
getCouponVMktModInfo    modParams          mktData       discCurve     index     startDate 
                        endDate            payDate       con           payOff    model 
                      = Ok (yearFrac, interpPayDate, model)
    where 
        yearFrac      = calcYearFrac startDate endDate (snd con)
        interpPayDate = 0.9          