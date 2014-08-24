{-# LANGUAGE DeriveDataTypeable #-}
module Interfaces.ILInterface   
    ( 
     buildJSON,    buildMarketJSON,
     buildDealInfo 
    ) where 

import qualified Data.Map as Map
import Control.Applicative
import Control.Monad 
import Vanilla.Types
import Vanilla.Instances
    
------------------------------ IL 2 JSON issues --------------------------------------         
-- First change ' by "
-- JSON only admits strings like keys, change key legs (0,0) and (0,1) by "(0,0)" and "(0,1)"
-- The same with CMS_DATES_DICT, change numbers by strings
-- The same with HISTORIC_FIXINGS_DICT
-- Substitute symbol ( of tuples with symbol [ of lists in operators.
-- In operators we have "" inside a string, we have to scape them

--------------------- Builders -------------------------------------------------------    
--------------------------------------------------------------------------------------
buildJSON :: String -> IO String
buildJSON path = do
    jsonIL <- readFile path
    let decodedIL = decode jsonIL :: Result (JSObject JSValue)   
    return (myEncodeJSON (buildInput =<< decodedIL))   
--------------------------------------------------------------------------------------
buildMarketJSON :: String -> IO String
buildMarketJSON path = do
    jsonIL <- readFile path
    let decodedIL = decode jsonIL :: Result (JSObject JSValue)   
    return (myEncodeJSON (buildMktParams =<< decodedIL))
--------------------------------------------------------------------------------------
buildInput :: JSObject JSValue -> Result Input
buildInput jsO = returnInput deal mktData modelParams
    where deal = buildDeal jsO
          mktData = buildMktParams jsO
          modelParams = buildModelParams jsO deal
          returnInput :: Result Deal -> Result MarketData -> 
                         Result ModelParameters -> Result Input 
          returnInput rDl rMkD rMdP = do
              dl <- rDl
              mkD <- rMkD
              mdP <- rMdP
              return (Input {inputDeal = dl, inputMarket = mkD, inputModelParams = mdP})
-- ----------------------------------          
buildDeal :: JSObject JSValue -> Result Deal
buildDeal jsO = returnDeal info prod
    where info = buildDealInfo jsO 
          prod = buildProduct jsO
          returnDeal :: Result DealInfo -> Result Product -> 
                        Result Deal
          returnDeal rDI rPr = do
              dI <- rDI
              pr <- rPr
              return Deal {dealInfo = dI, dealProduct = pr}
-- ------------------------------------------
buildProduct :: JSObject JSValue -> Result Product
buildProduct jsO = returnProduct rTP rED 
    where rTP = getValueJS jsO "DEAL/LEGS/(0, 0)/LOAN_PAYOUT" :: Result JSValue
          rED = getArrayJS jsO "DEAL/EXERCISE_DATES" :: Result [JSValue]
          returnProduct :: Result JSValue -> Result [JSValue] -> Result Product
          returnProduct rTP rED = do
              tP <- rTP    
              eD <- rED    
              let typeProduct = read (encode tP) :: Int
              case typeProduct of 0 -> buildSwapPlus jsO eD 
                                  x -> buildOption jsO 
              where       
                    buildSwapPlus :: JSObject JSValue -> [JSValue] -> Result Product
                    buildSwapPlus jsO eD
                        | (eD == [])  = buildSwap jsO
                        | otherwise = buildCancelSwap jsO exerDates                       
                            where exerDates = fmap excel2Day (read (encode eD) :: [Integer])
-- ------------------------------------------
buildCancelSwap :: JSObject JSValue -> [Day] -> Result Product
buildCancelSwap jsO eD = returnCancelSwap
    where returnCancelSwap = do 
              let sw = buildSwap jsO
              sw2 <- sw
              return CancelSwap { cacelSwap = sw2, exerciseDates = eD}

-- ------------------------------------------
buildSwap :: JSObject JSValue -> Result Product
buildSwap jsO = returnSwap typeLeg1 typeLeg2
    where typeLeg1 = getValueJS jsO "DEAL/LEGS/(0, 0)/LOAN_RATE_NATURE" :: Result JSValue
          typeLeg2 = getValueJS jsO "DEAL/LEGS/(0, 1)/LOAN_RATE_NATURE" :: Result JSValue
          returnSwap :: Result JSValue -> Result JSValue -> Result Product
          returnSwap rTl1 rTl2 = do
              tL1 <- rTl1 
              tL2 <- rTl2               
              returnSwap2 rL1 rL2
              where rL1 = buildLeg jsO 0
                    rL2 = buildLeg jsO 1
                    returnSwap2 :: Result Leg -> Result Leg -> Result Product 
                    returnSwap2 rL1 rL2 = do
                        l1 <- rL1
                        l2 <- rL2
                        return (Swap l1 l2 (AddFlows []) )  
-- ------------------------------------------
buildOption :: JSObject JSValue -> Result Product
buildOption jsO = returnOption typeLeg
    where typeLeg = getValueJS jsO "DEAL/LEGS/(0, 0)/LOAN_RATE_NATURE" :: Result JSValue
          returnOption :: Result JSValue -> Result Product
          returnOption rTl = do
              tL <- rTl 
              returnOption2 rL
              where rL = buildLeg jsO 0
                    returnOption2 :: Result Leg -> Result Product 
                    returnOption2 rL = do
                        l <- rL
                        return (Option l (AddFlows []) )  
                      
-- ------------------------------------------
buildLeg :: JSObject JSValue -> Int -> Result Leg
buildLeg jsO index = returnLeg rTypeLeg
    where path = "DEAL/LEGS/(0, " ++ (show index) ++ ")"          
          rTypeLeg = getFixedVariable >>= getTypeLeg
          returnLeg :: Result Int -> Result Leg
          returnLeg rTl = do
              tL <- rTl
              dispatch tL
          dispatch :: Int -> Result Leg
          dispatch 0 = buildFixedLeg jsO path
          dispatch 1 = buildLiborLeg jsO path
          dispatch 2 = buildCMSLeg jsO path
          rTypeLeg1 = getValueJS jsO (path ++ "/LOAN_RATE_NATURE") :: Result JSValue  
          getFixedVariable :: Result Int
          getFixedVariable = do
              typeLeg <- rTypeLeg1
              return (read (encode typeLeg) :: Int)
          getTypeLeg :: Int -> Result Int    
          getTypeLeg 0 = Ok 0
          getTypeLeg x = do 
              tL2 <- rTypeLeg2
              let tL2' = read (encode tL2) :: Int
              return (tL2' +1)
              where rTypeLeg2 = getValueJS jsO (path ++ "/INDEX_RATE_NATURE") :: Result JSValue
            
                    
-- ------------------------------------------
buildLiborLeg :: JSObject JSValue -> String -> Result Leg
buildLiborLeg jsO path = returnLiborLeg startDateCoup endDateCoup payDateCoup remCapCoup 
                                        basisCoup computeModeCoup fixingsLibor
                                        dataLibor basisLibor marginLibor 
                                        indexName discCurve payRec
                                        modelLabel typePayOff strikeLibor
    where startDateCoup = getArrayJS jsO (path ++ "/PERIOD_START_DATES") :: Result [JSValue]
          endDateCoup = getArrayJS jsO (path ++ "/PERIOD_END_DATES") :: Result [JSValue]
          payDateCoup = getArrayJS jsO (path ++ "/PERIOD_PAYMENT_DATES") :: Result [JSValue]
          remCapCoup = getArrayJS jsO (path ++ "/PERIOD_REMAINING_CAPITALS") :: Result [JSValue]
          basisCoup = getValueJS jsO (path ++ "/RATE_CONVENTION_BASIS") :: Result JSValue
          computeModeCoup = getValueJS jsO (path ++ "/RATE_CONVENTION_COMPUTING_MODE") :: Result JSValue
          fixingsLibor = getArrayJS jsO (path ++ "/PERIOD_FIXING_DATES") :: Result [JSValue]
          dataLibor = getLiborData jsO fixingsLibor path
          basisLibor = getValueJS jsO (path ++ "/INDEX_RATE_CONVENTION_BASIS") :: Result JSValue
          marginLibor = getArrayJS jsO (path ++ "/PERIOD_FLOATING_MARGINS") :: Result [JSValue]
          strikeLibor = getArrayJS jsO (path ++ "/PERIOD_STRIKE") :: Result [JSValue]
          indexName = getValueJS jsO (path ++ "/INDEX_NAME") :: Result JSValue
          discCurve = getTryValueJS jsO ["DEAL/DISCOUNT_CURVE","DEAL/DISCOUNTING_RATE_CURVES/EUR"] :: Result JSValue
          payRec = getValueJS jsO (path ++ "/LOAN_PAYOUT_SIGN") :: Result JSValue
          modelLabel = getValueJS jsO "DEAL/FLEX/0/BLOCK_LABEL" :: Result JSValue
          typePayOff = getValueJS jsO (path ++ "/LOAN_PAYOUT") :: Result JSValue
          returnLiborLeg :: Result [JSValue] -> Result [JSValue] -> Result [JSValue] ->
                            Result [JSValue] -> Result JSValue -> Result JSValue ->
                            Result [JSValue] -> Result ([JSValue], [JSValue], [JSValue]) -> 
                            Result JSValue -> Result [JSValue] ->
                            Result JSValue -> Result JSValue -> Result JSValue ->
                            Result JSValue -> Result JSValue -> Result [JSValue] -> Result Leg
          returnLiborLeg rSDC rEDC rPDC rRCC rBC rCMC rFL rDL rBL rML rIN rDC rPR rMoL rTPO rStr = do
              sDC <- rSDC 
              eDC <- rEDC 
              pDC <- rPDC 
              rCC <- rRCC 
              bC <- rBC 
              cMC <- rCMC 
              fL <- rFL 
              dL <- rDL 
              bL <- rBL
              mL <- rML 
              iN <- rIN 
              dC <- rDC 
              pR <- rPR 
              moL <- rMoL
              tPO <- rTPO
              str <- rStr
              let sDC2 = fmap excel2Day (read (encode sDC) :: [Integer]) 
              let eDC2 = fmap excel2Day (read (encode eDC) :: [Integer]) 
              let pDC2 = fmap excel2Day (read (encode pDC) :: [Integer]) 
              let rCC2 = read (encode rCC) :: [Double] 
              let bC2 = decodeBasis (read (encode bC) :: Int)
              let cMC2 = decodeComputeMode (read (encode cMC) :: Int)
              let fL2 = fmap excel2Day (read (encode fL) :: [Integer])     
              let sDL = (\(a,_,_) -> a) dL      
              let eDL = (\(_,a,_) -> a) dL      
              let pDL = (\(_,_,a) -> a) dL
              let sDL2 = fmap excel2Day (read (encode sDL) :: [Integer])     
              let eDL2 = fmap excel2Day (read (encode eDL) :: [Integer])     
              let pDL2 = fmap excel2Day (read (encode pDL) :: [Integer])      
              let bL2 = decodeBasis (read (encode bL) :: Int)
              let mL2 = fmap (/100) (read (encode mL) :: [Double])
              let iN2 = read (encode iN) :: String
              --iN3 <- decodeIndex iN2
              let dC2 = read (encode dC) :: String
              let pR2 = decodePayerReceiver (read (encode pR) :: Int)
              let moL2 = decodeModel (read (encode moL) :: String)
              let tPO2 = read (encode tPO) :: Int
              let str2 = fmap (/100) (read (encode str) :: [Double])
              let couponsList = getZipList $ buildCoupon <$> ZipList sDC2 <*> ZipList eDC2 <*> ZipList pDC2 
                                         <*> ZipList rCC2 <*> ZipList (repeat bC2) <*> ZipList (repeat cMC2)
                                         <*> ZipList fL2 <*> ZipList sDL2 <*> ZipList eDL2 <*> ZipList pDL2 
                                         <*> ZipList (repeat bL2) <*> ZipList mL2
                                         <*> ZipList (repeat moL2) <*> ZipList (repeat tPO2) <*> ZipList str2
                                         <*> ZipList (repeat iN2) <*> ZipList (repeat dC2)
              return (VariableLeg {coupons = couponsList, discCurve = dC2, legIndex = iN2, legPayerReceiver = pR2})
              where
                  buildCoupon sDC eDC pDC rCC bC cMC fL sDL eDL pDL bL mL mlL pOL str iN dC = returnCoupon sDC eDC pDC rCC bC cMC pO m
                      where 
                      pO = returnPayOff fL sDL eDL pDL bL mL pOL str
                      m = returnModel                    
                      returnCoupon sDC eDC pDC rCC bC cMC pO m = Variable {cpStartDate = sDC, cpEndDate = eDC, cpPayDate = pDC, 
                                                                   cpYearFrac = 0.0, cpRemainingCapital = rCC, cpConvention = (cMC, bC),
                                                                   varPayOff = pO, varModel = m, varNum0 = 0.0, cpDiscCurve = dC, cpIndex = iN}
                      returnPayOff fL sDL eDL pDL bL mL pOL str = case pOL of 0 -> Libor{liborFix = fL, liborStart = sDL, liborEnd = eDL, liborPay = pDL, 
                                                                                           liborConvention = (LIN, bL), margin = mL}
                                                                              1 -> Caplet{liborFix = fL, liborStart = sDL, liborEnd = eDL, liborPay = pDL, 
                                                                                     liborConvention = (LIN, bL), margin = mL, capStrike = str}
                                                                              2 -> Floorlet{liborFix = fL, liborStart = sDL, liborEnd = eDL, liborPay = pDL, 
                                                                                     liborConvention = (LIN, bL), margin = mL, floorStrike = str}      
                      returnModel = Forward {referenceDate = ModifiedJulianDay 0, forward = 0.0}
-- ------------------------------------------
getLiborData :: JSObject JSValue -> Result [JSValue] -> String -> 
                Result ([JSValue], [JSValue], [JSValue])
getLiborData jsO fixings path = returnLiborData fixings
    where returnLiborData :: Result [JSValue] -> 
                Result ([JSValue], [JSValue], [JSValue])
          returnLiborData rFx = do
              fx <- rFx
              let fx2 = read (encode fx) :: [Integer]
              let startDate = fmap (getInfoLibor "CALC_DATE_1") fx2 
              let endDate = fmap (getInfoLibor "CALC_DATE_2") fx2 
              let payDate = fmap (getInfoLibor "PAYMENT_DATE_2") fx2 
              startDate2 <- (checkAllOk startDate)
              endDate2 <- (checkAllOk endDate)
              payDate2 <- checkAllOk payDate
              return (startDate2, endDate2, payDate2)
          getInfoLibor :: String -> Integer -> Result JSValue              
          getInfoLibor field fx = getValueJS jsO (path ++ "/INDEX_COMPUTE_DATES/" ++ (show fx) ++ "/" ++ field) :: Result JSValue      
-- ------------------------------------------

buildCMSLeg :: JSObject JSValue -> String -> Result Leg
buildCMSLeg jsO path = returnCMSLeg startDateCoup endDateCoup payDateCoup remCapCoup 
                                        basisCoup computeModeCoup fixingsCMS
                                        datesCMS maturCMS basisCMS marginCMS 
                                        indexName discCurve payRec 
                                        modelLabel typePayOff strikeCMS
    where startDateCoup = getArrayJS jsO (path ++ "/PERIOD_START_DATES") :: Result [JSValue]
          endDateCoup = getArrayJS jsO (path ++ "/PERIOD_END_DATES") :: Result [JSValue]
          payDateCoup = getArrayJS jsO (path ++ "/PERIOD_PAYMENT_DATES") :: Result [JSValue]
          remCapCoup = getArrayJS jsO (path ++ "/PERIOD_REMAINING_CAPITALS") :: Result [JSValue]
          basisCoup = getValueJS jsO (path ++ "/RATE_CONVENTION_BASIS") :: Result JSValue
          computeModeCoup = getValueJS jsO (path ++ "/RATE_CONVENTION_COMPUTING_MODE") :: Result JSValue
          fixingsCMS = getArrayJS jsO (path ++ "/PERIOD_FIXING_DATES") :: Result [JSValue]
          datesCMS = getCMSDates jsO fixingsCMS path
          maturCMS = getValueJS jsO (path ++ "/INDEX_INSTRUMENT_MATURITY_LABEL") :: Result JSValue
          basisCMS = getValueJS jsO (path ++ "/INDEX_RATE_CONVENTION_BASIS") :: Result JSValue
          marginCMS = getArrayJS jsO (path ++ "/PERIOD_FLOATING_MARGINS") :: Result [JSValue]
          strikeCMS = getArrayJS jsO (path ++ "/PERIOD_STRIKE") :: Result [JSValue]
          indexName = getValueJS jsO (path ++ "/INDEX_NAME") :: Result JSValue
          discCurve = getTryValueJS jsO ["DEAL/DISCOUNT_CURVE","DEAL/DISCOUNTING_RATE_CURVES/EUR"] :: Result JSValue
          payRec = getValueJS jsO (path ++ "/LOAN_PAYOUT_SIGN") :: Result JSValue
          modelLabel = getValueJS jsO "DEAL/FLEX/0/BLOCK_LABEL" :: Result JSValue
          typePayOff = getValueJS jsO (path ++ "/LOAN_PAYOUT") :: Result JSValue
          returnCMSLeg :: Result [JSValue] -> Result [JSValue] -> Result [JSValue] ->
                            Result [JSValue] -> Result JSValue -> Result JSValue ->
                            Result [JSValue] -> Result [[JSValue]] -> Result JSValue -> 
                            Result JSValue -> Result [JSValue] -> Result JSValue -> 
                            Result JSValue -> Result JSValue -> Result JSValue -> 
                            Result JSValue -> Result [JSValue] ->Result Leg
          returnCMSLeg rSDC rEDC rPDC rRCC rBC rCMC rFCMS rDCMS rMtCMS rBCMS rMCMS rIN rDC rPR rML rTPO rStr = do
              sDC <- rSDC 
              eDC <- rEDC 
              pDC <- rPDC 
              rCC <- rRCC 
              bC <- rBC 
              cMC <- rCMC 
              fCMS <- rFCMS 
              dCMS <- rDCMS 
              mtCMS <- rMtCMS 
              bCMS <- rBCMS
              mCMS <- rMCMS 
              iN <- rIN 
              dC <- rDC 
              pR <- rPR
              mL <- rML
              tPO <- rTPO
              str <- rStr
              let sDC2 = fmap excel2Day (read (encode sDC) :: [Integer]) 
              let eDC2 = fmap excel2Day (read (encode eDC) :: [Integer]) 
              let pDC2 = fmap excel2Day (read (encode pDC) :: [Integer]) 
              let rCC2 = read (encode rCC) :: [Double] 
              let bC2 = decodeBasis (read (encode bC) :: Int)
              let cMC2 = decodeComputeMode (read (encode cMC) :: Int)
              let fCMS2 = fmap excel2Day (read (encode fCMS) :: [Integer])      
              let dCMS2 = fmap (fmap excel2Day) (read (encode dCMS) :: [[Integer]]) 
              let mtCMS2 = read (encode mtCMS) :: String    
              let bCMS2 = decodeBasis (read (encode bCMS) :: Int)
              let mCMS2 = fmap (/100) (read (encode mCMS) :: [Double])
              let iN2 = read (encode iN) :: String
              iN3 <- decodeIndex iN2
              let dC2 = read (encode dC) :: String
              let pR2 = decodePayerReceiver (read (encode pR) :: Int)
              let mL2 = decodeModel (read (encode mL) :: String)
              let tPO2 = read (encode tPO) :: Int
              let str2 = fmap (/100) (read (encode str) :: [Double])
              let couponsList = getZipList $ buildCoupon <$> ZipList sDC2 <*> ZipList eDC2 <*> ZipList pDC2 
                                         <*> ZipList rCC2 <*> ZipList (repeat bC2) <*> ZipList (repeat cMC2)
                                         <*> ZipList fCMS2 <*> ZipList dCMS2 <*> ZipList (repeat mtCMS2)
                                         <*> ZipList (repeat bCMS2) <*> ZipList mCMS2
                                         <*> ZipList (repeat mL2) <*> ZipList (repeat tPO2) <*> ZipList str2
                                         <*> ZipList (repeat iN2) <*> ZipList (repeat dC2)
              return (VariableLeg {coupons = couponsList, discCurve = dC2, legIndex = iN2, legPayerReceiver = pR2})
              where
                  buildCoupon sDC eDC pDC rCC bC cMC fCMS dCMS mtCMS bCMS mCMS mlCMS poCMS str iN dC = returnCoupon sDC eDC pDC rCC bC cMC pO mlCMS
                      where 
                      pO = returnPayOff fCMS dCMS mtCMS bCMS mCMS poCMS str                    
                      returnCoupon sDC eDC pDC rCC bC cMC pO m = Variable {cpStartDate = sDC, cpEndDate = eDC, cpPayDate = pDC, 
                                                                   cpYearFrac = 0.0, cpRemainingCapital = rCC, cpConvention = (cMC, bC),
                                                                   varPayOff = pO, varModel = m, varNum0 = 0.0, cpDiscCurve = dC, cpIndex = iN}
                      returnPayOff fCMS dCMS mtCMS bCMS mCMS poCMS str = case poCMS of 0 -> CMS{cmsFix = fCMS, cmsDates = dCMS, cmsMaturity = mtCMS,
                                                                                        cmsConvention = (LIN, bCMS), cmsMargin = mCMS}
                                                                                       1 -> CapletCMS{cmsFix = fCMS, cmsDates = dCMS, cmsMaturity = mtCMS,
                                                                                        cmsConvention = (LIN, bCMS), cmsMargin = mCMS, capStrike = str}
                                                                                       2 -> FloorletCMS{cmsFix = fCMS, cmsDates = dCMS, cmsMaturity = mtCMS,
                                                                                        cmsConvention = (LIN, bCMS), cmsMargin = mCMS, floorStrike = str}      
-- ------------------------------------------
getCMSDates :: JSObject JSValue -> Result [JSValue] -> String -> 
                Result [[JSValue]]
getCMSDates jsO fixings path = returnCMSDates fixings
    where returnCMSDates :: Result [JSValue] ->  Result [[JSValue]]
          returnCMSDates rFx = do
              fx <- rFx
              let fx2 = read (encode fx) :: [Integer]
              let dates = fmap getDatesCMS fx2 
              dates2 <- (checkAllOk dates)
              return dates2
          getDatesCMS :: Integer -> Result [JSValue]              
          getDatesCMS fx = getArrayJS jsO (path ++ "/CMS_DATES_DICT/" ++ (show fx)) :: Result [JSValue]      
-- ------------------------------------------

buildFixedLeg :: JSObject JSValue -> String -> Result Leg
buildFixedLeg jsO path = returnFixedLeg startDateCoup endDateCoup payDateCoup remCapCoup 
                                        ratesCoup basisCoup computeModeCoup discCurve payRec
    where startDateCoup = getArrayJS jsO (path ++ "/PERIOD_START_DATES") :: Result [JSValue]
          endDateCoup = getArrayJS jsO (path ++ "/PERIOD_END_DATES") :: Result [JSValue]
          payDateCoup = getArrayJS jsO (path ++ "/PERIOD_PAYMENT_DATES") :: Result [JSValue]
          remCapCoup = getArrayJS jsO (path ++ "/PERIOD_REMAINING_CAPITALS") :: Result [JSValue]
          basisCoup = getValueJS jsO (path ++ "/RATE_CONVENTION_BASIS") :: Result JSValue
          computeModeCoup = getValueJS jsO (path ++ "/RATE_CONVENTION_COMPUTING_MODE") :: Result JSValue
          ratesCoup = getArrayJS jsO (path ++ "/PERIOD_APPLICABLE_RATES") :: Result [JSValue]
          discCurve = getTryValueJS jsO ["DEAL/DISCOUNT_CURVE","DEAL/DISCOUNTING_RATE_CURVES/EUR"] :: Result JSValue
          payRec = getValueJS jsO (path ++ "/LOAN_PAYOUT_SIGN") :: Result JSValue
          returnFixedLeg :: Result [JSValue] -> Result [JSValue] -> Result [JSValue] ->
                            Result [JSValue] -> Result [JSValue] -> Result JSValue ->
                            Result JSValue -> Result JSValue -> Result JSValue -> Result Leg
          returnFixedLeg rSDC rEDC rPDC rRCC rRC rBC rCMC rDC rPR = do
              sDC <- rSDC 
              eDC <- rEDC 
              pDC <- rPDC 
              rCC <- rRCC 
              rC <- rRC 
              bC <- rBC 
              cMC <- rCMC 
              dC <- rDC 
              pR <- rPR 
              let sDC2 = fmap excel2Day (read (encode sDC) :: [Integer]) 
              let eDC2 = fmap excel2Day (read (encode eDC) :: [Integer]) 
              let pDC2 = fmap excel2Day (read (encode pDC) :: [Integer]) 
              let rCC2 = read (encode rCC) :: [Double] 
              let rC2 = fmap (/100) (read (encode rC) :: [Double]) 
              let bC2 = decodeBasis (read (encode bC) :: Int)
              let cMC2 = decodeComputeMode (read (encode cMC) :: Int)
              let dC2 = read (encode dC) :: String
              let pR2 = decodePayerReceiver (read (encode pR) :: Int)
              let couponsList = getZipList $ buildCoupon <$> ZipList sDC2 <*> ZipList eDC2 <*> ZipList pDC2 
                                         <*> ZipList rCC2 <*> ZipList rC2 <*> ZipList (repeat bC2) 
                                         <*> ZipList (repeat cMC2) <*> ZipList (repeat dC2)
              return (FixedLeg {coupons = couponsList, discCurve = dC2, legPayerReceiver = pR2})
              where
                  buildCoupon sDC eDC pDC rCC rC bC cMC dC = returnCoupon sDC eDC pDC rCC rC bC cMC dC                     
                  returnCoupon sDC eDC pDC rCC rC bC cMC dC = Fixed {cpStartDate = sDC, cpEndDate = eDC, cpPayDate = pDC, 
                                                                      cpYearFrac = 0.0, cpRemainingCapital = rCC, fxRate = rC,
                                                                      cpConvention = (cMC, bC), fxDiscFactor = 0.0, cpDiscCurve = dC}
-- ------------------------------------------
{-buildDealInfo :: JSObject JSValue -> Result DealInfo
buildDealInfo jsO = returnDealInfo portf evDate isPr flxType blckLabel
    where portf = getValueJS jsO "DEAL/DEAL_PORTFOLIO_LABEL" :: Result JSValue
          evDate = getValueJS jsO "DEAL/DEAL_TRADE_SETTLEMENT_DATE" :: Result JSValue
          isPr = getValueJS jsO "DEAL/DEAL_IN_PRICING_ENVIRONMENT" :: Result JSValue
          flxType = getValueJS jsO "DEAL/FLEX/0/TYPE" :: Result JSValue
          blckLabel = getValueJS jsO "DEAL/FLEX/0/BLOCK_LABEL" :: Result JSValue
          returnDealInfo :: Result JSValue -> Result JSValue -> Result JSValue ->
                            Result JSValue -> Result JSValue -> Result DealInfo
          returnDealInfo (Ok pf) (Ok eD) (Ok iP) (Ok fT) (Ok bL) = 
              Ok DealInfo {portfolio = pf2, evalDate = eD2, 
                isPricingMode = if (iP2 == 1) then True else False, flex = flx}
              where pf2 = read (encode pf) :: String
                    eD2 = excel2Day (read (encode eD) :: Integer)
                    iP2 = read (encode iP) :: Int
                    fT2 = read (encode fT) :: String
                    bL2 = read (encode bL) :: String
                    flx = Flex {flexType = fT2, blockLabel = bL2}  
          returnDealInfo a b c d e = Error ((getErr a) ++ (getErr b) ++ (getErr c) ++ (getErr d) ++ (getErr e))
-}          
          
buildDealInfo :: JSObject JSValue -> Result DealInfo
buildDealInfo jsO = returnDealInfo portf evDate isPr flxType blckLabel
    where portf = getValueJS jsO "DEAL/DEAL_PORTFOLIO_LABEL" :: Result JSValue
          evDate = getValueJS jsO "DEAL/DEAL_TRADE_SETTLEMENT_DATE" :: Result JSValue
          isPr = getValueJS jsO "DEAL/DEAL_IN_PRICING_ENVIRONMENT" :: Result JSValue
          flxType = getValueJS jsO "DEAL/FLEX/0/TYPE" :: Result JSValue
          blckLabel = getValueJS jsO "DEAL/FLEX/0/BLOCK_LABEL" :: Result JSValue
          returnDealInfo :: Result JSValue -> Result JSValue -> Result JSValue ->
                            Result JSValue -> Result JSValue -> Result DealInfo
          returnDealInfo rPf rED rIP rFT rBL = do              
              pf <- rPf
              eD <- rED
              iP <- rIP
              fT <- rFT
              bL <- rBL       
              let pf2 = read (encode pf) :: String
              let eD2 = excel2Day (read (encode eD) :: Integer)
              let iP2 = read (encode iP) :: Int
              let fT2 = read (encode fT) :: String
              let bL2 = read (encode bL) :: String
              let flx = Flex {flexType = fT2, blockLabel = bL2}          
              return (DealInfo {portfolio = pf2, evalDate = eD2, 
                        isPricingMode = if (iP2 == 1) then True else False, flex = flx})
-- ------------
buildModelParams :: JSObject JSValue -> Result Deal -> Result ModelParameters
buildModelParams jsO rDeal = returnModelParams params 
    where params = buildParams jsO rDeal
          returnModelParams :: Result [Parameters] -> Result ModelParameters
          returnModelParams rParams = do
              params <- rParams
              return ModelParameters {parameters = params}
-- ------------
buildParams :: JSObject JSValue -> Result Deal -> Result [Parameters]
buildParams jsO rDeal = do
    deal <- rDeal
    let product = dealProduct deal    
    let rParameters = case product of Swap l1 l2 _                     -> do p1 <- (buildParamsLeg jsO l1 [])
                                                                             p2 <- (buildParamsLeg jsO l2 p1)
                                                                             return (p1 ++ p2)
                                      Option l _                       -> do p <- (buildParamsLeg jsO l [])
                                                                             return p
                                      CancelSwap (Swap l1 l2 _ ) ed    -> do p1 <- (buildParamsLeg jsO l1 [])
                                                                             p2 <- (buildParamsLeg jsO l2 p1)
                                                                             return (p1 ++ p2)
    parameters <- rParameters                                                            
    return parameters
    where buildParamsLeg :: JSObject JSValue -> Leg -> [Parameters] -> Result [Parameters] 
          buildParamsLeg jsO VariableLeg {coupons = cs, discCurve = dC,
                        legIndex = lI, legPayerReceiver = pR} ps = buildModParamsPerModel jsO (varModel (cs!!0)) lI ps
          buildParamsLeg jsO x y = Ok []                
-- ------------
buildModParamsPerModel :: JSObject JSValue -> Model -> String -> [Parameters] -> Result [Parameters]
buildModParamsPerModel jsO (HaganRepSABRRBS2 {}) index params = returnModParams rAtmVolExp rAtmVolTen rAtmVolMatrix
                                                                                rSabrExp rSabrTen rBetaMatrix rRhoMatrix rVolOfVolMatrix
                                                                                rRbs2ExtrapExp rRbs2ExtrapTen rRbs2KMinusExtrapExp rRbs2KMinusExtrapTen
                                                                                rRbs2KExtrapExp rRbs2KExtrapTen rRbs2LeftExtrapMatrix
                                                                                rRbs2LeftStrikeMatrix rRbs2RightExtrapMatrix rRbs2RightStrikeMatrix
                                                                                rKappaTen rKappaValues
    where rAtmVolExp = if checkSabr then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/ATMVOL_EXPIRIES") :: Result [JSValue]
          rAtmVolTen = if checkSabr then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/ATMVOL_TENORS") :: Result [JSValue]
          rAtmVolMatrix = if checkSabr then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/ATMVOL_MATRIX") :: Result [[JSValue]] 
          rSabrExp = if checkSabr then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/SABR_EXPIRIES") :: Result [JSValue]
          rSabrTen = if checkSabr then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/SABR_TENORS") :: Result [JSValue]
          rBetaMatrix = if checkSabr then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/BETA_MATRIX") :: Result [[JSValue]] 
          rRhoMatrix = if checkSabr then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/RHO_MATRIX") :: Result [[JSValue]] 
          rVolOfVolMatrix = if checkSabr then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/VOLOFVOL_MATRIX") :: Result [[JSValue]]           
          rRbs2ExtrapExp = if checkRBS2 then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/RBS2_EXTRAP_EXPIRIES") :: Result [JSValue]
          rRbs2ExtrapTen = if checkRBS2 then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/RBS2_EXTRAP_TENORS") :: Result [JSValue] 
          rRbs2KMinusExtrapExp = if checkRBS2 then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/RBS2_KMINUS_EXTRAP_EXPIRIES") :: Result [JSValue]
          rRbs2KMinusExtrapTen = if checkRBS2 then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/RBS2_KMINUS_EXTRAP_TENORS") :: Result [JSValue]
          rRbs2KExtrapExp = if checkRBS2 then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/RBS2_K_EXTRAP_EXPIRIES") :: Result [JSValue] 
          rRbs2KExtrapTen = if checkRBS2 then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/RBS2_K_EXTRAP_TENORS") :: Result [JSValue] 
          rRbs2LeftExtrapMatrix = if checkRBS2 then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/RBS2_LEFT_EXTRAP_MATRIX") :: Result [[JSValue]] 
          rRbs2LeftStrikeMatrix = if checkRBS2 then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/RBS2_LEFT_STRIKE_MATRIX") :: Result [[JSValue]]
          rRbs2RightExtrapMatrix = if checkRBS2 then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/RBS2_RIGHT_EXTRAP_MATRIX") :: Result [[JSValue]] 
          rRbs2RightStrikeMatrix = if checkRBS2 then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/RBS2_RIGHT_STRIKE_MATRIX") :: Result [[JSValue]]
          rKappaTen = if checkKappa then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/KAPPA_TENORS") :: Result [JSValue] 
          rKappaValues = if checkKappa then Ok [] else getArrayJS jsO ("MODEL_PARAMETERS/KAPPA_VALUES") :: Result [JSValue] 
          checkSabr = (length (filter fSabr params)) > 0 where fSabr SABR {paramsIndex=index,vatmSABR=_,betaSABR=_,rhoSABR=_,volOfVolSABR=_} = True
                                                               fSabr x = False
          checkRBS2 = (length (filter fRbs2 params)) > 0 where fRbs2 RBS2 {paramsIndex=index,rightStrike=_,rightParams=_,leftStrike=_,leftParams=_} = True
                                                               fRbs2 x = False
          checkKappa = (length (filter fKappa params)) > 0 where fKappa KAPPA {paramsIndex=index,kTenors=_,kValues=_} = True
                                                                 fKappa x = False
          returnModParams :: Result [JSValue] -> Result [JSValue] -> Result [[JSValue]] -> Result [JSValue] ->
                             Result [JSValue] -> Result [[JSValue]] -> Result [[JSValue]] -> Result [[JSValue]] ->
                             Result [JSValue] -> Result [JSValue] -> Result [JSValue] -> Result [JSValue] ->
                             Result [JSValue] -> Result [JSValue] -> Result [[JSValue]] -> Result [[JSValue]] ->
                             Result [[JSValue]] -> Result [[JSValue]] -> Result [JSValue] -> Result [JSValue] ->
                             Result [Parameters]
          returnModParams rAtmVolExp rAtmVolTen rAtmVolMatrix
                          rSabrExp rSabrTen rBetaMatrix rRhoMatrix rVolOfVolMatrix
                          rRbs2ExtrapExp rRbs2ExtrapTen rRbs2KMinusExtrapExp rRbs2KMinusExtrapTen
                          rRbs2KExtrapExp rRbs2KExtrapTen rRbs2LeftExtrapMatrix
                          rRbs2LeftStrikeMatrix rRbs2RightExtrapMatrix rRbs2RightStrikeMatrix 
                          rKappaTen rKappaValues = do --
              atmVolExp <- rAtmVolExp
              atmVolTen <- rAtmVolTen
              atmVolMatrix <- rAtmVolMatrix
              sabrExp <- rSabrExp
              sabrTen <- rSabrTen
              betaMatrix <- rBetaMatrix
              rhoMatrix <- rRhoMatrix
              volOfVolMatrix <- rVolOfVolMatrix      
              rbs2ExtrapExp <- rRbs2ExtrapExp
              rbs2ExtrapTen <- rRbs2ExtrapTen
              rbs2KMinusExtrapExp <- rRbs2KMinusExtrapExp
              rbs2KMinusExtrapTen <- rRbs2KMinusExtrapTen
              rbs2KExtrapExp <- rRbs2KExtrapExp
              rbs2KExtrapTen <- rRbs2KExtrapTen
              rbs2LeftExtrapMatrix <- rRbs2LeftExtrapMatrix
              rbs2LeftStrikeMatrix <- rRbs2LeftStrikeMatrix
              rbs2RightExtrapMatrix <- rRbs2RightExtrapMatrix
              rbs2RightStrikeMatrix <- rRbs2RightStrikeMatrix
              kappaTen <- rKappaTen
              kappaValues <- rKappaValues
              
              let atmVolExp2 = read (encode atmVolExp) :: [Double]
              let atmVolTen2 = read (encode atmVolTen) :: [Double] 
              let atmVolMatrix2 = read (encode atmVolMatrix) :: [[Double]]
              let sabrExp2 = read (encode sabrExp) :: [Double] 
              let sabrTen2 = read (encode sabrTen) :: [Double] 
              let betaMatrix2 = read (encode betaMatrix) :: [[Double]]
              let rhoMatrix2 = read (encode rhoMatrix) :: [[Double]]      
              let volOfVolMatrix2 = read (encode volOfVolMatrix) :: [[Double]]   
              let rbs2ExtrapExp2 = read (encode rbs2ExtrapExp) :: [Double]
              let rbs2ExtrapTen2 = read (encode rbs2ExtrapTen) :: [Double]
              let rbs2KMinusExtrapExp2 = read (encode rbs2KMinusExtrapExp) :: [Double]
              let rbs2KMinusExtrapTen2 = read (encode rbs2KMinusExtrapTen) :: [Double]
              let rbs2KExtrapExp2 = read (encode rbs2KExtrapExp) :: [Double]
              let rbs2KExtrapTen2 = read (encode rbs2KExtrapTen) ::[Double]
              let rbs2LeftExtrapMatrix2 = read (encode rbs2LeftExtrapMatrix) :: [[Double]]   
              let rbs2LeftStrikeMatrix2 = read (encode rbs2LeftStrikeMatrix) :: [[Double]]   
              let rbs2RightExtrapMatrix2 = read (encode rbs2RightExtrapMatrix) :: [[Double]]   
              let rbs2RightStrikeMatrix2 = read (encode rbs2RightStrikeMatrix) :: [[Double]]   
              let kappaTen2 = read (encode kappaTen) :: [Double]
              let kappaValues2 = read (encode kappaValues) :: [Double]
              
              let sabr = SABR {paramsIndex = index, 
                               vatmSABR = ParamsMatrix{expiries = atmVolExp2,tenors = atmVolTen2, matrix = atmVolMatrix2}, 
                               betaSABR = ParamsMatrix{expiries = sabrExp2,tenors = sabrTen2, matrix = betaMatrix2}, 
                               rhoSABR = ParamsMatrix{expiries = sabrExp2,tenors = sabrTen2, matrix = rhoMatrix2}, 
                               volOfVolSABR = ParamsMatrix{expiries = sabrExp2,tenors = sabrTen2, matrix = volOfVolMatrix2}}
              let rbs2 = RBS2 {paramsIndex = index, 
                               rightStrike = ParamsMatrix{expiries = rbs2KExtrapExp2,tenors = rbs2KExtrapTen2, matrix = rbs2RightStrikeMatrix2}, 
                               rightParams = ParamsMatrix{expiries = rbs2ExtrapExp2,tenors = rbs2ExtrapTen2, matrix = rbs2RightExtrapMatrix2}, 
                               leftStrike = ParamsMatrix{expiries = rbs2KMinusExtrapExp2,tenors = rbs2KMinusExtrapTen2, matrix = rbs2LeftStrikeMatrix2}, 
                               leftParams = ParamsMatrix{expiries = rbs2ExtrapExp2,tenors = rbs2ExtrapTen2, matrix = rbs2LeftExtrapMatrix2}}
              
              let kappa = KAPPA {paramsIndex = index, kTenors = kappaTen2, kValues = kappaValues2} 
              let checkSabr = (length (filter fSabr params)) > 0 where fSabr SABR {paramsIndex=index,vatmSABR=_,betaSABR=_,rhoSABR=_,volOfVolSABR=_} = True
                                                                       fSabr x = False
              let checkRBS2 = (length (filter fRbs2 params)) > 0 where fRbs2 RBS2 {paramsIndex=index,rightStrike=_,rightParams=_,leftStrike=_,leftParams=_} = True
                                                                       fRbs2 x = False
              let checkKappa = (length (filter fKappa params)) > 0 where fKappa KAPPA {paramsIndex=index,kTenors=_,kValues=_} = True
                                                                         fKappa x = False
              return   ((if checkSabr then [] else [sabr]) ++ 
                       (if checkRBS2 then [] else [rbs2]) ++ 
                       (if checkKappa then [] else [kappa]) )
              
buildModParamsPerModel jsO model index params = Ok [] 
           
-- ------------
buildMktParams :: JSObject JSValue -> Result MarketData
buildMktParams    jsO               = returnMktData curves volasCf volasSw histFix
    where curves  = buildCurves      jsO
          volasCf = buildVolCapFloor jsO
          volasSw = buildVolSwaption jsO
          histFix = buildHistFix     jsO
          --returnMktData :: Result [RateCurve] -> Result [CapFloorVolGenerator] -> Result [SwaptionVolGenerator] -> Result (Map.Map String HistoricFixings)
          returnMktData :: Result [RateCurve] -> Result [CapFloorVolGenerator] -> Result [SwaptionVolGenerator] -> Result [HistoricFixings]
                        -> Result MarketData
          returnMktData    rCvs                  rVlsCF                           rVlsSw                           rHistFix
                        = do
              cvs     <- rCvs
              vlsCF   <- rVlsCF
              vlsSw   <- rVlsSw
              histFix <- rHistFix
              return MarketData {curves = cvs, capFloorVols = vlsCF, swaptionVols = vlsSw, historicFix = histFix}
--------------------------------------------------------------------------------------
{-buildHistFix :: JSObject JSValue -> Result (Map.Map String HistoricFixings)
buildHistFix    jsO               = returnHFix indx1 indx2
    where 
          indx1 = getValueJS jsO "DEAL/LEGS/(0, 0)/INDEX_NAME" :: Result JSValue
          indx2 = getValueJS jsO "DEAL/LEGS/(0, 1)/INDEX_NAME" :: Result JSValue
          returnHFix :: Result JSValue -> Result JSValue -> Result (Map.Map String HistoricFixings)
          returnHFix    (Ok i1)           (Ok i2)         = fromList' $ checkAllOk lResult
              where 
                    nIndex1  = read (encode i1) :: String
                    nIndex2  = read (encode i2) :: String
                    lResult1 = buildHistFix' jsO 0 nIndex1
                    lResult2 = buildHistFix' jsO 1 nIndex2
                    lResult  = [lResult1, lResult2] 
          returnHFix    (Ok i1)            y              = fromList' $ checkAllOk lResult
              where 
                    nIndex1  = read (encode i1) :: String
                    lResult1 = buildHistFix' jsO 0 nIndex1
                    lResult  = [lResult1] 
          returnHFix     x                 (Ok i2)        = fromList' $ checkAllOk lResult
              where 
                    nIndex2  = read (encode i2) :: String
                    lResult2 = buildHistFix' jsO 1 nIndex2
                    lResult  = [lResult2] 
          returnHFix     x                 y              = Error ((getErr x) ++ (getErr y))
          fromList' lR = do
              l <- lR
              return (Map.fromList l)
--------------------------------------------------------------------------------------
buildHistFix' :: JSObject JSValue -> Int   -> String    -> Result (String, HistoricFixings)
buildHistFix'    jsO                 index    indexName  = returnHFix jsO rFixingsDates
   where 
         path = "DEAL/LEGS/(0, " ++ (show index) ++ ")"
         rFixingsDates = getArrayJS jsO (path ++ "/PERIOD_FIXING_DATES") :: Result [JSValue]
         returnHFix :: JSObject JSValue -> Result [JSValue] -> Result (String, HistoricFixings)
         returnHFix jsO rFixingsDates = do
             fixingsDates <- rFixingsDates
             let fixingsDates2' = read (encode fixingsDates) :: [Int] 
             let fixingsDates2 = fmap show fixingsDates2'
             values <- getOnlyOk (fmap (getValueJS jsO) (fmap ((path ++ "/HISTORIC_FIXINGS_DICT/") ++) fixingsDates2) :: [Result JSValue])
             let values2 = [0.0,0.9]--read (encode values) :: [Double]
             let resultZL = [(excel2Day 48000,0.0)]--zip (fmap (excel2Day . read) fixingsDates2) values2
             return (indexName, Map.fromList resultZL)      -}
--------------------------------------------------------------------------------------
buildHistFix :: JSObject JSValue -> Result [HistoricFixings]
buildHistFix    jsO               = returnHFix indx1 indx2
    where 
          indx1 = getValueJS jsO "DEAL/LEGS/(0, 0)/INDEX_NAME" :: Result JSValue
          indx2 = getValueJS jsO "DEAL/LEGS/(0, 1)/INDEX_NAME" :: Result JSValue
          returnHFix :: Result JSValue -> Result JSValue -> Result [HistoricFixings]
          returnHFix    (Ok i1)           (Ok i2)         = checkAllOk lResult
              where 
                    nIndex1  = read (encode i1) :: String
                    nIndex2  = read (encode i2) :: String
                    lResult1 = buildHistFix' jsO 0 nIndex1
                    lResult2 = buildHistFix' jsO 1 nIndex2
                    lResult  = [lResult1, lResult2] 
          returnHFix    (Ok i1)            y              = checkAllOk lResult
              where 
                    nIndex1  = read (encode i1) :: String
                    lResult1 = buildHistFix' jsO 0 nIndex1
                    lResult  = [lResult1] 
          returnHFix     x                 (Ok i2)        = checkAllOk lResult
              where 
                    nIndex2  = read (encode i2) :: String
                    lResult2 = buildHistFix' jsO 1 nIndex2
                    lResult  = [lResult2] 
          returnHFix     x                 y              = Error ((getErr x) ++ (getErr y))
--------------------------------------------------------------------------------------
buildHistFix' :: JSObject JSValue -> Int   -> String    -> Result HistoricFixings
buildHistFix'    jsO                 index    indexName  = returnHFix jsO rFixingsDates
   where 
         path = "DEAL/LEGS/(0, " ++ (show index) ++ ")"
         rFixingsDates = getArrayJS jsO (path ++ "/PERIOD_FIXING_DATES") :: Result [JSValue]
         returnHFix :: JSObject JSValue -> Result [JSValue] -> Result HistoricFixings
         returnHFix jsO rFixingsDates = do
             fixingsDates <- rFixingsDates
             let fixingsDates2' = read (encode fixingsDates) :: [Int] 
             let fixingsDates2 = fmap show fixingsDates2'
             values <- getOnlyOk (fmap (getValueJS jsO) (fmap ((path ++ "/HISTORIC_FIXINGS_DICT/") ++) fixingsDates2) :: [Result JSValue])
             let values2 = read (encode values) :: [Double]
             return HistoricFixings{hfIndex = indexName, hfDates = (fmap (excel2Day . read) fixingsDates2), hfValues = values2}         
              
--------------------------------------------------------------------------------------
buildVolSwaption :: JSObject JSValue -> Result [SwaptionVolGenerator]
buildVolSwaption jsO = returnVols curr
    where curr = [EUR]
          returnVols :: [Currency] -> Result [SwaptionVolGenerator]
          returnVols c = checkAllOk lResultVols
              where lResultVols = fmap (buildVolSwaption1 jsO) c
-- -------
buildVolSwaption1 :: JSObject JSValue -> Currency -> Result SwaptionVolGenerator
buildVolSwaption1 jsO curr = returnVolSw matrix optMat strikes swapMat
   where path = "MARKET_PARAMETERS/SWAPTION_VOLATILITIES/" ++ (show curr) ++ "-IBOR"
         matrix = getArrayJS jsO (path ++ "/MATRIX") 
                     :: Result [[[JSValue]]]
         optMat = getArrayJS jsO (path ++ "/OPTION_MATURITIES") :: Result [JSValue]
         strikes = getArrayJS jsO (path ++ "/STRIKES") :: Result [JSValue]
         swapMat = getArrayJS jsO (path ++ "/SWAP_MATURITIES") :: Result [JSValue]
         returnVolSw :: Result [[[JSValue]]] -> Result [JSValue] ->
                      Result [JSValue] -> Result [JSValue] -> Result SwaptionVolGenerator
         returnVolSw (Ok m) (Ok oM) (Ok s) (Ok sM) = 
             Ok (SwVInterpolator curr LINEAR_INT SwaptionVol {swMatrix = m2, swOptMat = oM2, swStrikes = s2, swSwapMat = sM2})
             where m2 = fmap (fmap (fmap (/100))) (read (encode m) :: [[[Double]]])
                   oM2 = fmap excel2Day (read (encode oM) :: [Integer])
                   s2 = fmap (/100) (read (encode s) :: [Double])
                   sM2 = read (encode sM) :: [[Int]]
         returnVolSw a b c d = Error ((getErr a) ++ (getErr b) ++ (getErr c) ++ (getErr d))

-- ------------
buildVolCapFloor :: JSObject JSValue -> Result [CapFloorVolGenerator]
buildVolCapFloor jsO = returnVols indx1 indx2
    where indx1 = getValueJS jsO "DEAL/LEGS/(0, 0)/INDEX_NAME" :: Result JSValue
          indx2 = getValueJS jsO "DEAL/LEGS/(0, 1)/INDEX_NAME" :: Result JSValue
          returnVols :: Result JSValue -> Result JSValue -> Result [CapFloorVolGenerator]
          returnVols (Ok i1) (Ok i2) = checkAllOk lResultVols
              where lResultVols = fmap (buildVolCapFloor1 jsO) lStringIndx
                    lStringIndx = [read (encode i1) :: String, 
                                   read (encode i2) :: String] 
          returnVols (Ok i1) y = checkAllOk lResultCvs
              where lResultCvs = fmap (buildVolCapFloor1 jsO) lStringIndx
                    lStringIndx = [read (encode i1) :: String]
          returnVols x (Ok i2) = checkAllOk lResultCvs
              where lResultCvs = fmap (buildVolCapFloor1 jsO) lStringIndx
                    lStringIndx = [read (encode i2) :: String]
          returnVols x y = Error ((getErr x) ++ (getErr y))
-----------
buildVolCapFloor1 :: JSObject JSValue -> String -> Result CapFloorVolGenerator
buildVolCapFloor1 jsO index = returnVolCF matrix optMat strikes
   where path = "MARKET_PARAMETERS/CAPFLOOR_VOLATILITIES/" ++ index
         matrix = getArrayJS jsO (path ++ "/MATRIX") 
                     :: Result [[JSValue]]
         optMat = getArrayJS jsO (path ++ "/OPTION_MATURITIES") :: Result [JSValue]
         strikes = getArrayJS jsO (path ++ "/STRIKES") :: Result [JSValue]
         returnVolCF :: Result [[JSValue]] -> Result [JSValue] ->
                      Result [JSValue] -> Result CapFloorVolGenerator
         returnVolCF (Ok m) (Ok oM) (Ok s) = 
             Ok (CFVInterpolator index LINEAR_INT (CapFloorVol {cfvMatrix = m2,
                                                        cfvOptMat = oM2, cfvStrikes = s2}))
             where m2 = fmap (fmap (/100)) (read (encode m) :: [[Double]])
                   oM2 = fmap excel2Day (read (encode oM) :: [Integer])
                   s2 = fmap (/100) (read (encode s) :: [Double])
         returnVolCF a b c = Error ((getErr a) ++ (getErr b) ++ (getErr c))

-- ------------
buildCurves :: JSObject JSValue -> Result [RateCurve]
buildCurves jsO = returnCurves discCurve estCurves
    where discCurve = getDiscountCurve jsO
          estCurves = getEstimationCurves jsO
          curr = EUR
          returnCurves :: Result (JSValue,JSValue) -> Result [(JSValue,JSValue)] -> Result [RateCurve]
          returnCurves (Ok dCurve) (Ok estCurves) = checkAllOk lResultCvs
              where lResultCvs = fmap (buildCurve jsO curr) lStringCvs
                    lStringCvs = strDCurve : strEstCurves
                    strDCurve =  (read (encode $ fst dCurve) :: String,read (encode $ snd dCurve) :: String) 
                    strEstCurves =  zip (read (encode $ fmap fst estCurves) :: [String]) (read (encode $ fmap snd estCurves) :: [String]) 
          returnCurves x y = Error ((getErr x) ++ (getErr y))
-- --------------------------------   
getDiscountCurve :: JSObject JSValue -> Result (JSValue,JSValue)
getDiscountCurve jsO = returnCurve rDiscCurve
    where
        returnCurve :: Result JSValue -> Result (JSValue,JSValue)
        returnCurve rCurve = do
            curve <- rCurve
            return (curve, JSString (JSONString ""))
        rDiscCurve = getTryValueJS jsO ["DEAL/DISCOUNT_CURVE","DEAL/DISCOUNTING_RATE_CURVES/EUR"] :: Result JSValue
        
        
-- --------------------------------   
getEstimationCurves :: JSObject JSValue -> Result [(JSValue,JSValue)]
getEstimationCurves jsO = returnCurves indexes
    where index1 = (error2EmptyList (getValueJS jsO "DEAL/LEGS/(0, 0)/INDEX_NAME" :: Result JSValue)) :: [Result JSValue]
          index2 = (error2EmptyList (getValueJS jsO "DEAL/LEGS/(0, 1)/INDEX_NAME" :: Result JSValue)) :: [Result JSValue]
          indexes = index1 ++ index2
          returnCurves :: [Result JSValue] -> Result [(JSValue,JSValue)]
          returnCurves indexes = checkAllOk (fmap getCurve indexes)
          getCurve :: Result JSValue -> Result (JSValue, JSValue) 
          getCurve rInd = do
              ind <- rInd
              let ind2 = read (encode ind) :: String
              let rCurve = getValueJS jsO ("DEAL/DICT_INDEX_RATECURVE/" ++ ind2)  :: Result JSValue 
              curve <- rCurve
              return (curve, ind)

-- --------------------------------                          
buildCurve :: JSObject JSValue -> Currency -> (String, String) -> Result RateCurve
buildCurve jsO curr nameAndIndex = returnCurve coumpConv num den bCurve intForm
                                   rfDt sprFormula val2Int mats dfs
   where path = "MARKET_PARAMETERS/DISCOUNT_FACTOR_CURVE/" 
                ++ (show curr) ++ "/" ++ (fst nameAndIndex)
         coumpConv = getValueJS jsO (path ++ "/CONVENTION_COMPUTING_MODE") 
                     :: Result JSValue
         num = getValueJS jsO (path ++ "/CONVENTION_NUMERATOR") :: Result JSValue
         den = getValueJS jsO (path ++ "/CONVENTION_DENOMINATOR") 
               :: Result JSValue
         bCurve = getValueJS jsO (path ++ "/BASE_CURVE") :: Result JSValue
         intForm = getValueJS jsO (path ++ "/INTERPOLATION_FORMULA") 
                   :: Result JSValue
         rfDt = getValueJS jsO (path ++ "/REFERENCE_DATE") :: Result JSValue
         sprFormula = getValueJS jsO (path ++ "/SPREAD_CURVE_FORMULA") 
                      :: Result JSValue
         val2Int = getValueJS jsO (path ++ "/VALUE_TO_INTERPOLATE") 
                   :: Result JSValue
         mats = getArrayJS jsO (path ++ "/PILLAR_MATURITIES") :: Result [JSValue]
         dfs = getArrayJS jsO (path ++ "/DISCOUNT_FACTORS") :: Result [JSValue]
         returnCurve :: Result JSValue -> Result JSValue -> Result JSValue ->
                        Result JSValue -> Result JSValue -> Result JSValue ->
                        Result JSValue -> Result JSValue -> Result [JSValue] ->
                        Result [JSValue] -> Result RateCurve
         returnCurve (Ok cC) (Ok n) (Ok d) (Ok bC) (Ok iF) 
                     (Ok rD) (Ok sF) (Ok vI) (Ok ms) (Ok dFs) = 
             Ok RateCurve {curveName = (fst nameAndIndex), 
             curveIndex = (maybeIndex $ snd nameAndIndex), 
             currencyCurve = curr,
             conventionCurve = conv, baseCurve = bC2,
             discountFactors = dFs2, interpolationFormula = iF2,
             pillarMaturities = ms2, refDate = rD2, 
             spreadCurveFormula = sF2, value2Interpolate = vI2}
             where conv = (read cC2 :: CoumpondingConvention, fracConv)
                   fracConv = read (n2 ++ d2) :: FracConvention
                   cC2 = read (encode cC) :: String
                   n2 = read (encode n) :: String
                   d2 = read (encode d) :: String
                   bC2 = read (encode bC) :: String
                   iF2 = read (encode iF) :: String
                   rD2 = excel2Day (read (encode rD) :: Integer)
                   sF2 = read (encode sF) :: String
                   vI2 = read (encode vI) :: String
                   ms2 = fmap excel2Day (read (encode ms) :: [Integer])
                   dFs2 = read (encode dFs) :: [Double]
                   maybeIndex :: String -> Maybe String
                   maybeIndex "" = Nothing
                   maybeIndex x = Just x
         returnCurve a b c d e f g h i j = Error ((getErr a) ++ (getErr b) ++ (getErr c) ++ (getErr d) ++ (getErr e) ++ (getErr f) ++ (getErr g) ++ (getErr h) ++ (getErr i) ++ (getErr j)) 


--------------------------------------------------------
decodeBasis :: Int -> FracConvention
decodeBasis 0 = ACT360
decodeBasis 1 = ACT360
decodeBasis 2 = ACT360
decodeBasis 3 = ACT360
decodeBasis 4 = ACT360
decodeBasis 5 = ACT360

decodeComputeMode :: Int -> CoumpondingConvention
decodeComputeMode 0 = LIN

decodePayerReceiver :: Int -> PayerReceiver
decodePayerReceiver 0 = PAYER
decodePayerReceiver 1 = RECEIVER

decodeModel :: String -> Model
decodeModel "VANILLACMS" = HaganRepSABRRBS2 {referenceDate = ModifiedJulianDay 0, kappa    = 0.0, forward = 0.0, 
                                             vAtm          = 0.0,                 beta     = 0.0, rho     = 0.0,
                                             volOfVol      = 0.0,                 xPlus    = 0.0, xMinus  = 0.0, 
                                             nu            = 0.0,                 mu       = 0.0, expiry  = 0.0}

decodeIndex :: String -> Result Index
decodeIndex indexName = if searchResult == [] then Error ("There is not an index called " ++ indexName)
                                              else (Ok (searchResult!!0))
    where searchResult = filter (\x -> ((iName x) == indexName)) listIndex

                 
    
    
    
