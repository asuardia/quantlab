{-# LANGUAGE DeriveDataTypeable #-}
module Vanilla.SAGreeks   
    ( 
     GreeksContainer (..)   ,MarketContainer (..) ,        RateCurveContainer (..),        CapFloorVolContainer (..),          SwaptionVolContainer (..),
     ModelParamsContainer (..), ParamsContainer (..), Container1 , Container1', Container2, Container2', Container3, Mappeable, mapG
    ) where

--import Data.Map    
import Data.Monoid   
import Data.List
import Vanilla.Types 
import Vanilla.Instances

type Container1  = (Pillar, Value)
type Container1' = (Tenor, Value)
type Container2  = (Expiry, Strike, Value)
type Container2' = (Expiry, Tenor, Value)
type Container3  = (Expiry, Tenor, Strike, Value)
                          
type Pillar = Day
type Tenor = Double
type Expiry = Day
type Strike = Double
type Value  = Double

data GreeksContainer = GreeksContainer {
                                           gMktGreeks       :: MarketContainer,
                                           gModParamsGreeks :: ModelParamsContainer
                                       } deriving (Eq, Show, Data, Typeable)

-- Market Data  
--------------------------------------------------------------------------------------
data MarketContainer = MarketContainer {
                                           gCurves       :: [RateCurveContainer], 
                                           gCapFloorVols :: [CapFloorVolContainer],
                                           gSwaptionVols :: [SwaptionVolContainer]
                                       } deriving (Eq, Show, Data, Typeable)
  
--------------------------------------------------------------------------------------
data RateCurveContainer = RateCurveContainer {
                                                 gCurveName   :: String, 
                                                 gCurveIndex  :: Maybe String,
                                                 gCurveValues :: [Container1]
                                             } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------------------
data CapFloorVolContainer = CapFloorVolContainer {
                                                     gCFIndex  :: String,
                                                     gCFValues :: [Container2]
                                                 } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------------------
data SwaptionVolContainer = SwaptionVolContainer {
                                                     gSwCurr   :: Currency, 
                                                     gSwValues :: [Container3]
                                                 } deriving (Eq, Show, Data, Typeable)
  
--------------------------------------------------------------------------------------

-- Model Parameters
data ModelParamsContainer = ModelParamsContainer { 
                                                     gParams :: [ParamsContainer]
                                                 } deriving (Eq, Show, Data, Typeable)
 
--------------------------------------------------------------------------------------
-- Parameters
data ParamsContainer = SABRContainer  { 
                                          gParamsIndex  :: String, 
                                          gVatmSABR     :: [Container2'], 
                                          gBetaSABR     :: [Container2'], 
                                          gRhoSABR      :: [Container2'], 
                                          gVolOfVolSABR :: [Container2']
                                      } 
                     | RBS2Container  { 
                                          gParamsIndex :: String, 
                                          gRightStrike :: [Container2'], 
                                          gRightParams :: [Container2'],
                                          gLeftStrike  :: [Container2'], 
                                          gLeftParams  :: [Container2']
                                      } 
                     | KAPPAContainer {   
                                          gparamsIndex :: String, 
                                          gValues      :: [Container1']
                                      } deriving (Eq, Show, Data, Typeable)
             

 
--------------------------------------------------------------------------------------
-- Mappeable class
class Mappeable m where
    mapG :: ModelParameters -> MarketData -> ValueStorage -> m -> Result GreeksContainer  
 
--------------------------------------------------------------------------------------
instance Mappeable ProductReady2Val where
    mapG modParams mktData vS (ProductReady2Val {getR2ValProduct = Ready2Val (Swap g l1 l2 af)}) = do
        grC1 <- mapG modParams mktData ((subValues vS)!!0) (LegReady2Val (Ready2Val (l1)))
        grC2 <- mapG modParams mktData ((subValues vS)!!1) (LegReady2Val (Ready2Val (l2)))
        return (mappend grC1 grC2)
    mapG modParams mktData vS ProductReady2Val {getR2ValProduct = Ready2Val (Option g l af)} = do
        grC1 <- mapG modParams mktData ((subValues vS)!!0) (LegReady2Val (Ready2Val (l)))
        return grC1       
    mapG modParams mktData vS x = Error "There are not semi-analytical greeks for this product."
--------------------------------------------------------------------------------------
instance Mappeable LegReady2Val where
    mapG modParams mktData vS (LegReady2Val (Ready2Val (l))) = do
        listCpnsCont <- checkAllOk $ fmap mapG' tuplaCsVs  
        return (mconcat listCpnsCont)                                       
        where 
              mapG' tupla = mapG modParams mktData (fst tupla) (snd tupla)
              rVCoupons   = (fmap CouponReady2Val (fmap Ready2Val (coupons l)))
              tuplaCsVs   = zip (subValues vS) rVCoupons
--------------------------------------------------------------------------------------
instance Mappeable CouponReady2Val where
    mapG modParams mktData vS (CouponReady2Val (Ready2Val cp)) = Ok (mapGreeks cp vS)
    
--------------------------------------------------------------------------------------
instance Monoid GreeksContainer where  
    mempty = GreeksContainer mempty mempty
    mappend (GreeksContainer mk1 md1) (GreeksContainer mk2 md2) = GreeksContainer (mappend mk1 mk2) (mappend md1 md2)
--------------------------------------------------------------------------------------
instance Monoid MarketContainer where  
    mempty = MarketContainer [] [] []
    mappend (MarketContainer rcc1 vcfc1 vswc1 ) (MarketContainer rcc2 vcfc2 vswc2 ) = 
        MarketContainer (groupRCC (rcc1 ++ rcc2)) (groupCFVC (vcfc1 ++ vcfc2)) (groupSWVC (vswc1 ++ vswc2))
        where
              groupRCC :: [RateCurveContainer] -> [RateCurveContainer]
              groupRCC lsRCC = fmap group (nCurves lsRCC)
                  where 
                        nCurves lsrcc = nub $ fmap gCurveName lsrcc
                        group name = RateCurveContainer {gCurveValues = concat $ fmap gCurveValues sameCurve, 
                                                         gCurveIndex  = gCurveIndex (sameCurve!!0), 
                                                         gCurveName   = name}
                            where
                                sameCurve = filter (\rcc -> name == gCurveName rcc) lsRCC
              groupCFVC :: [CapFloorVolContainer] -> [CapFloorVolContainer]
              groupCFVC lsCFVC = fmap group (iCFVols lsCFVC)
                  where 
                        iCFVols lscfv = nub $ fmap gCFIndex lscfv
                        group index = CapFloorVolContainer {gCFValues = concat $ fmap gCFValues sameCFV, 
                                                            gCFIndex  = index}
                            where
                                sameCFV = filter (\cfvc -> index == gCFIndex cfvc) lsCFVC
              groupSWVC :: [SwaptionVolContainer] -> [SwaptionVolContainer]
              groupSWVC lsSWVC = fmap group (cSWVols lsSWVC)
                  where 
                        cSWVols lsswv = nub $ fmap gSwCurr lsswv
                        group curr = SwaptionVolContainer {gSwValues = concat $ fmap gSwValues sameSWV, 
                                                            gSwCurr  = curr}
                            where
                                sameSWV = filter (\swvc -> curr == gSwCurr swvc) lsSWVC
--------------------------------------------------------------------------------------
instance Monoid ModelParamsContainer where  
    mempty = ModelParamsContainer [] 
    mappend (ModelParamsContainer pc1) (ModelParamsContainer pc2 ) = 
        ModelParamsContainer (pc1 ++ pc2) 


--------------------------------------------------------------------------------------
mapGreeks :: Coupon -> ValueStorage -> GreeksContainer
mapGreeks cp vs = GreeksContainer (mapMktGreeks cp vs) (mapModParamsGreeks cp vs)
--------------------------------------------------------------------------------------
mapMktGreeks :: Coupon -> ValueStorage -> MarketContainer
mapMktGreeks cp vs = MarketContainer (mapRateCurveGreeks cp vs) (mapCFVolGreeks cp vs) (mapSwVolGreeks cp vs) 
--------------------------------------------------------------------------------------
mapRateCurveGreeks :: Coupon -> ValueStorage                      -> [RateCurveContainer]
mapRateCurveGreeks    Fixed {cpDiscCurve = dc, cpPayDate = pd}                      
                      vs                                           = [RateCurveContainer {gCurveName = dc, gCurveIndex = Nothing, gCurveValues = [(pd, (greeks vs) !! 0)]}]
--------------------------------------------------------------------------------------
mapRateCurveGreeks   Variable {cpDiscCurve = dc, cpIndex = i,
                     cpPayDate = pd, 
                     varPayOff = Libor fx st nd py cn mr,
                     varModel = Forward rD f}                      
                     vs                                            = [RateCurveContainer {gCurveName = dc, gCurveIndex = Nothing, gCurveValues = [(pd, (greeks vs) !! 0)]},
                                                                      RateCurveContainer {gCurveName = dc, gCurveIndex = Just i,  gCurveValues = [(pd, (greeks vs) !! 1)]}]
--------------------------------------------------------------------------------------

mapRateCurveGreeks   Variable {cpDiscCurve = dc, cpIndex = i,
                     cpPayDate = pd, 
                     varPayOff = Libor fx st nd py cn mr,
                     varModel = ForwardNonStandard rD e t2P f v}   
                     vs                                            = [RateCurveContainer {gCurveName = dc, gCurveIndex = Nothing, gCurveValues = [(pd, (greeks vs) !! 0)]},
                                                                      RateCurveContainer {gCurveName = dc, gCurveIndex = Just i,  gCurveValues = [(pd, (greeks vs) !! 1)]}]
--------------------------------------------------------------------------------------

mapRateCurveGreeks   Variable {cpDiscCurve = dc, cpIndex = i,
                     cpPayDate = pd, 
                     varPayOff = Caplet fx st nd py cn mr k,
                     varModel = Black rD e v f}                     
                     vs                                            = [RateCurveContainer {gCurveName = dc, gCurveIndex = Nothing, gCurveValues = [(pd, (greeks vs) !! 0)]},
                                                                      RateCurveContainer {gCurveName = dc, gCurveIndex = Just i,  gCurveValues = [(pd, (greeks vs) !! 1)]}]
--------------------------------------------------------------------------------------

mapRateCurveGreeks   Variable {cpDiscCurve = dc, cpIndex = i,
                     cpPayDate = pd, 
                     varPayOff = Floorlet fx st nd py cn mr k,
                     varModel = Black rD e v f}                     
                     vs                                            = [RateCurveContainer {gCurveName = dc, gCurveIndex = Nothing, gCurveValues = [(pd, (greeks vs) !! 0)]},
                                                                      RateCurveContainer {gCurveName = dc, gCurveIndex = Just i,  gCurveValues = [(pd, (greeks vs) !! 1)]}]
--------------------------------------------------------------------------------------

mapRateCurveGreeks   Variable {cpDiscCurve = dc, cpIndex = i,
                     cpPayDate = pd, 
                     varPayOff = Caplet fx st nd py cn mr k,
                     varModel = BlackNonStandard rD e t2P v f vAd}  
                     vs                                            = [RateCurveContainer {gCurveName = dc, gCurveIndex = Nothing, gCurveValues = [(pd, (greeks vs) !! 0)]},
                                                                      RateCurveContainer {gCurveName = dc, gCurveIndex = Just i,  gCurveValues = [(pd, (greeks vs) !! 1)]}]
--------------------------------------------------------------------------------------

mapRateCurveGreeks   Variable {cpDiscCurve = dc, cpIndex = i,
                     cpPayDate = pd, 
                     varPayOff = Floorlet fx st nd py cn mr k,
                     varModel = BlackNonStandard rD e t2P v f vAd}                     
                     vs                                            = [RateCurveContainer {gCurveName = dc, gCurveIndex = Just i, gCurveValues = [(pd, (greeks vs) !! 0)]},
                                                                      RateCurveContainer {gCurveName = dc, gCurveIndex = Just i, gCurveValues = [(pd, (greeks vs) !! 1)]}]
--------------------------------------------------------------------------------------

mapRateCurveGreeks   Variable {cpDiscCurve = dc, cpIndex = i,
                     cpPayDate = pd, 
                     varPayOff = CMS fx ds mt cn mr,
                     varModel = HaganRepSABRRBS2 rD e  f  v b r 
                                                 vv xp xM n m k}                     
                     vs                                            = [RateCurveContainer {gCurveName = dc, gCurveIndex = Nothing,  gCurveValues = [(pd, (greeks vs) !! 0)]},
                                                                      RateCurveContainer {gCurveName = dc, gCurveIndex = Just "i", gCurveValues = [(pd, (greeks vs) !! 1)]}]
--------------------------------------------------------------------------------------
mapRateCurveGreeks   Variable {cpDiscCurve = dc, cpIndex = i,
                     cpPayDate = pd, 
                     varPayOff = CapletCMS fx ds mt cn mr str,
                     varModel = HaganRepSABRRBS2 rD e  f  v b r 
                                                 vv xp xM n m k}                     
                     vs                                            = [RateCurveContainer {gCurveName = dc, gCurveIndex = Nothing,  gCurveValues = [(pd, (greeks vs) !! 0)]},
                                                                      RateCurveContainer {gCurveName = dc, gCurveIndex = Just "i", gCurveValues = [(pd, (greeks vs) !! 1)]}]
--------------------------------------------------------------------------------------
mapRateCurveGreeks   Variable {cpDiscCurve = dc, cpIndex = i,
                     cpPayDate = pd, 
                     varPayOff = FloorletCMS fx ds mt cn mr str,
                     varModel = HaganRepSABRRBS2 rD e  f  v b r 
                                                 vv xp xM n m k}                     
                     vs                                            = [RateCurveContainer {gCurveName = dc, gCurveIndex = Nothing,  gCurveValues = [(ModifiedJulianDay 0, 1.0)]},
                                                                      RateCurveContainer {gCurveName = dc, gCurveIndex = Just "i", gCurveValues = [(ModifiedJulianDay 0, 1.0)]}]
--------------------------------------------------------------------------------------
mapRateCurveGreeks   cp         vs                                 = []
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
mapCFVolGreeks :: Coupon -> ValueStorage                       -> [CapFloorVolContainer]
--------------------------------------------------------------------------------------

mapCFVolGreeks    Variable {cpDiscCurve = dc, cpIndex = i,
                  cpPayDate = pd, 
                  varPayOff = Libor fx st nd py cn mr,
                  varModel = ForwardNonStandard rD e t2P f v}   
                  vs                                            = [CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx, f, (greeks vs) !! 2)]}]
--------------------------------------------------------------------------------------

mapCFVolGreeks    Variable {cpDiscCurve = dc, cpIndex = i,
                  cpPayDate = pd, 
                  varPayOff = Caplet fx st nd py cn mr k,
                  varModel = Black rD e v f}                     
                  vs                                            = [CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx, k, (greeks vs) !! 2)]}]
--------------------------------------------------------------------------------------

mapCFVolGreeks    Variable {cpDiscCurve = dc, cpIndex = i,
                  cpPayDate = pd, 
                  varPayOff = Floorlet fx st nd py cn mr k,
                  varModel = Black rD e v f}                     
                  vs                                            = [CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx, k,(greeks vs) !! 2)]}]
--------------------------------------------------------------------------------------

mapCFVolGreeks    Variable {cpDiscCurve = dc, cpIndex = i,
                  cpPayDate = pd, 
                  varPayOff = Caplet fx st nd py cn mr k,
                  varModel = BlackNonStandard rD e t2P v f vAd}  
                  vs                                            = [CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx, k, (greeks vs) !! 2)]},
                                                                   CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx, f, (greeks vs) !! 3)]}]
--------------------------------------------------------------------------------------

mapCFVolGreeks    Variable {cpDiscCurve = dc, cpIndex = i,
                  cpPayDate = pd, 
                  varPayOff = Floorlet fx st nd py cn mr k,
                  varModel = BlackNonStandard rD e t2P v f vAd}                     
                  vs                                            = [CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx, k, (greeks vs) !! 2)]},
                                                                   CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx, f, (greeks vs) !! 3)]}]
--------------------------------------------------------------------------------------
mapCFVolGreeks    cp        vs                                  = []
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

mapSwVolGreeks :: Coupon -> ValueStorage -> [SwaptionVolContainer]
mapSwVolGreeks    cp        vs            = []
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

mapModParamsGreeks :: Coupon -> ValueStorage -> ModelParamsContainer
mapModParamsGreeks cp vs = ModelParamsContainer []        
        
        
        
        
        
        
        

                          
                          
                          
                          
                          
                          
                          
                          
                          
