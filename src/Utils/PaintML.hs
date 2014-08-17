module Utils.PaintML   
    ( 
     ML, paint2ML
    ) where

import Data.List
import Data.Data
import Data.Typeable
import Vanilla.Types

--------------------------------------------------------------------------------------
class ML a where
    paint2ML :: a -> String    

instance ML Product where
    paint2ML Swap       {generator = _, swLeg1 = l1, swLeg2 = l2, addFlows = _} = paint2ML l1 ++ paint2ML l2   
    paint2ML CancelSwap {cacelSwap = swCan, exerciseDates = eD}                 = paint2ML swCan ++ 
        "bloqueFechasCancel=" 
        ++ show (fmap day2Matlab eD) 
        ++ "';flagsModelo=[100;1];flagsVega=[0;0;0];" 
    paint2ML Option     {generator = _, opLeg = l, addFlows = _}                = paint2ML l    
--------------------------------------------------------------------------------------
instance ML Leg where
    paint2ML FixedLeg {coupons = cs, discCurve = _, legPayerReceiver = pR}                                      = 
        "bloquePF=[" 
        ++ (concat $ fmap paint2ML cs) 
        ++ "];" 
        ++ flags
        where 
              flags = "flagsPF=[" 
                      ++ (show $ codeMLPayerReceiver pR) 
                      ++ " " ++ (show $ codeMLFracConvention (snd $ cpConvention (cs!!0))) 
                      ++ "];"
    paint2ML vL 
        | isInfixOf (show $ toConstr $ varPayOff $ (coupons vL)!!0) ("Libor"++"Caplet"++"Floorlet")     == True = 
            "bloquePV=[" 
            ++ (concat $ fmap paint2ML $ coupons vL) 
            ++ "];" 
            ++ (flagsPV vL)
        | isInfixOf (show $ toConstr $ varPayOff $ (coupons vL)!!0) ("CMS"++"CapletCMS"++"FloorletCMS") == True = 
            "bloquePCMS=[" 
            ++ (concat $ fmap paint2ML $ coupons vL) 
            ++ "];" 
            ++ (flagsPCMS vL) 
            ++ (paint2MLCMS $ coupons vL)    
--------------------------------------------------------------------------------------
flagsPV (VariableLeg cs _ _ pR) = "flagsPV=[" 
                                  ++ (show $ codeMLPayerReceiver pR) ++ " " 
                                  ++ (show $ codeMLFracConvention (snd $ liborConvention $ varPayOff $ (cs!!0))) ++ " " 
                                  ++ (show $ codeMLCoumpModConvention (fst $ liborConvention $ varPayOff $ (cs!!0))) ++ " " 
                                  ++ (show $ codeMLFracConvention (snd $ cpConvention (cs!!0))) ++ " 0 3];"    
--------------------------------------------------------------------------------------
flagsPCMS (VariableLeg cs _ _ pR) = "flagsPCMS=[" 
                                    ++ (show $ codeMLPayerReceiver pR) ++ " " 
                                    ++ (show $ codeMLFracConvention (snd $ cmsConvention $ varPayOff $ (cs!!0))) ++ " " 
                                    ++ (show $ codeMLCoumpModConvention (fst $ cmsConvention $ varPayOff $ (cs!!0))) ++ " " 
                                    ++ (show $ codeMLFracConvention (snd $ cpConvention (cs!!0))) ++ " 0 3];"    
--------------------------------------------------------------------------------------
instance ML Coupon where
    paint2ML Fixed    {cpStartDate = sD, cpEndDate = eD,          cpPayDate = pD, 
                       cpYearFrac = yF,  cpRemainingCapital = rC, cpConvention = cnv,
                       fxRate = fR,      fxDiscFactor = dF} = 
        (show $ day2Matlab sD) ++ " " ++ (show $ day2Matlab eD) ++ " " ++ (show rC) ++ " " ++ (show fR) ++ " " ++ (show $ day2Matlab pD) ++ ";"
    paint2ML Variable {cpStartDate = sD, cpEndDate = eD,          cpPayDate = pD, 
                       cpYearFrac = yF,  cpRemainingCapital = rC, cpConvention = cnv,
                       varPayOff = Libor { liborFix = lf,        liborStart = ls, liborEnd = le, liborPay = lp, 
                                           liborConvention = lc, margin = lm},
                       varModel = m,     varNum0 = n0}      =
        (show $ day2Matlab sD) ++ " " ++ (show $ day2Matlab eD) ++ " " ++ (show rC) ++ " " 
        ++ (show $ day2Matlab lf) ++ " " ++ (show $ day2Matlab ls) ++ " " ++ (show $ day2Matlab le) ++ " 0 0 " ++ (show lm) ++ " " ++ (show $ day2Matlab pD) ++ ";"  
    paint2ML Variable {cpStartDate = sD, cpEndDate = eD,          cpPayDate = pD, 
                       cpYearFrac = yF,  cpRemainingCapital = rC, cpConvention = cnv,
                       varPayOff = CMS {cmsFix = cf, cmsDates = cd, cmsConvention = cc, cmsMargin = cm},
                       varModel = m,     varNum0 = n0}      =
        (show $ day2Matlab sD) ++ " " ++ (show $ day2Matlab eD) ++ " " ++ (show rC) ++ " " 
        ++ (show $ day2Matlab cf) ++ " " ++ " 0 0 " ++ (show cm) ++ " " ++ (show $ day2Matlab pD) ++ ";"      
--------------------------------------------------------------------------------------
paint2MLCMS :: [Coupon] -> String
paint2MLCMS    cs       =  "bloqueFechasIniPFPCMS = [" ++ (concat $ fmap (paintDts 0) cs) ++ "];" 
                           ++ "bloqueFechasFinPFPCMS = [" ++ (concat $ fmap (paintDts 1) cs) ++ "];" 
                           ++ "bloqueFechasIniPVPCMS = [" ++ (concat $ fmap (paintDts 0) cs) ++ "];" 
                           ++ "bloqueFechasFinPVPCMS = [" ++ (concat $ fmap (paintDts 1) cs) ++ "];" 
    where 
          paintDts pos c = (concat $ (trunc pos) (fmap paintDt dts)) ++ ";" 
              where 
                    dts       = cmsDates $ varPayOff $ c
                    paintDt d = (show $ day2Matlab d) ++ " "    
                    trunc 0   = init
                    trunc 1   = tail    
--------------------------------------------------------------------------------------
instance ML MarketData where
    paint2ML mkt = cvs ++ vols ++ swVols
        where 
              cvs    = concat $ fmap paint2ML (curves mkt)
              vols   = concat $ fmap paint2ML (capFloorVols mkt)
              swVols = concat $ fmap paint2ML (swaptionVols mkt)    
--------------------------------------------------------------------------------------
instance ML RateCurve where
    paint2ML cv = filter (\x -> x /= '-') $ (curveName cv) ++ " = [" 
                  ++ concat (zipWith paintPair (pillarMaturities cv) (discountFactors cv)) ++ "];"
        where
              paintPair :: Day -> Double -> String      
              paintPair    pm     df     =  show (day2Matlab pm) ++ " " ++ show df ++ ";"    
--------------------------------------------------------------------------------------
instance ML SwaptionVol where
    paint2ML v = strikes ++ exp ++ ten ++ (concat $ fmap paintSheet iv)
        where
              iv = zip [1,2..] (swMatrix v)
              paintSheet :: (Int, [[Double]]) -> String      
              paintSheet    ish               =  "SwaptionVol(" ++ show (fst ish) ++ ",:,:) = [" 
                                                 ++ (concat $ fmap paintSheet2 (snd ish)) ++ "];"
              paintSheet2   l                 = (concat $ fmap (++" ") (fmap show l)) ++ ";"
              strikes                         = "swStrikes=" ++ (show (swStrikes v)) ++ "';"
              exp                             = "swSwapMat=" ++ (show $ (swSwapMat v)!!0) ++ "';"
              ten                             = "swOptMat=" ++ (show $ fmap day2Matlab (swOptMat v)) ++ "';"    
--------------------------------------------------------------------------------------
instance ML CapFloorVol where
    paint2ML v =  strikes ++ exp ++ "VolCF_" ++ (filter (\x -> x /= '-') $ (cfvIndex v)) ++ " = [" 
                  ++ (concat $ fmap paintSheet2 (cfvMatrix v)) ++ "];"
        where
              paintSheet2 l = (concat $ fmap (++" ") (fmap show l)) ++ ";"
              strikes       = "cfvStrikes=" ++ (show (cfvStrikes v)) ++ "';"
              exp           = "cfvOptMat=" ++ (show $ fmap day2Matlab (cfvOptMat v)) ++ "';"    
--------------------------------------------------------------------------------------
codeMLPayerReceiver :: PayerReceiver -> Int            
codeMLPayerReceiver    PAYER         =  1           
codeMLPayerReceiver    RECEIVER      =  0    
--------------------------------------------------------------------------------------
codeMLFracConvention :: FracConvention -> Int           
codeMLFracConvention    ACTACT         =  0           
codeMLFracConvention    THIRTY360      =  1         
codeMLFracConvention    ACT360         =  2           
codeMLFracConvention    ACT365         =  3    
--------------------------------------------------------------------------------------
codeMLCoumpModConvention :: CoumpondingConvention -> Int           
codeMLCoumpModConvention    LIN                   =  0           
codeMLCoumpModConvention    YIELD                 =  1     













