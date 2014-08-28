{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
module Vanilla.Containers   
    ( 
     GreeksContainer (..),      MarketContainer (..),        
     CapFloorVolContainer (..), SwaptionVolContainer (..),
     RateCurveContainer (..),   ModelParamsContainer (..), 
     ParamsContainer (..),      Container1 , 
     Container1',               Container2, 
     Container2',               Container3, 
     Mappeable,                 mapG,
     Viewer (..),               ValueContainer (..), 
     mapV  
    ) where

-------------------------------------------------------------------------- 
import qualified Data.Monoid as Mo  
import Data.Time.Calendar
import Data.List
import Utils.MyJSON
import Utils.MyUtils
import Vanilla.Types 
import Vanilla.Models
import Vanilla.PayOffs
import Vanilla.Instances
import Vanilla.ModelParameters
import Market.FinantialConventions
import Market.MarketData

-------------------------------------------------------------------------- 
type Container1  = (Pillar, Value)
type Container1' = (Tenor, Value)
type Container2  = (Expiry, Strike, Value)
type Container2' = (Expiry, Tenor, Value)
type Container3  = (Expiry, Tenor, Strike, Value)
                          
-------------------------------------------------------------------------- 
type Pillar = Day
type Tenor  = Double
type Expiry = Day
type Strike = Double
type Value  = Double

--------------------------------------------------------------------------
data Viewer = Viewer { 
                         valueResult  :: ValueContainer, 
                         greeksResult :: GreeksContainer
                     } deriving (Eq, Show, Data, Typeable)   

--------------------------------------------------------------------------
data ValueContainer = ValueSwap   {
                                      valueCont     :: Double, 
                                      valueLeg1     :: Double,
                                      valueLeg2     :: Double,
                                      valueCpnsLeg1 :: [Double],
                                      valueCpnsLeg2 :: [Double]                            
                                  }
                    | ValueOption { 
                                      valueCont :: Double,
                                      valueCpns :: [Double]
                                  }      
                    | ValueLeg { 
                                      valueCont :: Double,
                                      valueCpns :: [Double]
                                  }           
                    | ValuePortf { 
                                      valueCont :: Double,
                                      valuePrs  :: [Double]
                                  }      
                    | Value       {
                                      valueCont :: Double
                                  } deriving (Eq, Show, Data, Typeable)   

-------------------------------------------------------------------------- 
data GreeksContainer = GreeksContainer {
                                           gMktGreeks       :: MarketContainer,
                                           gModParamsGreeks :: ModelParamsContainer
                                       } deriving (Eq, Show, Data, Typeable)


--------------------------------------------------------------------------
data MarketContainer = MarketContainer {
                                           gCurves       :: [RateCurveContainer], 
                                           gCapFloorVols :: [CapFloorVolContainer],
                                           gSwaptionVols :: [SwaptionVolContainer]
                                       } deriving (Eq, Show, Data, Typeable)
  
--------------------------------------------------------------------------
data RateCurveContainer = RateCurveContainer {
                                                 gCurveName   :: String, 
                                                 gCurveValues :: [Container1]
                                             } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data CapFloorVolContainer = CapFloorVolContainer {
                                                     gCFIndex  :: String,
                                                     gCFValues :: [Container2]
                                                 } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data SwaptionVolContainer = SwaptionVolContainer {
                                                     gSwCurr   :: Currency, 
                                                     gSwValues :: [Container3]
                                                 } deriving (Eq, Show, Data, Typeable)
  
--------------------------------------------------------------------------

-- Model Parameters
data ModelParamsContainer = ModelParamsContainer { 
                                                     gParams :: [ParamsContainer]
                                                 } deriving (Eq, Show, Data, Typeable)
 
--------------------------------------------------------------------------
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
             

 
--------------------------------------------------------------------------
-- Mappeable class
class Mappeable m where
    mapG :: ModelParameters -> MarketData -> ValueStorage -> m 
            -> Result GreeksContainer  
    mapV :: ValueStorage -> m -> Result ValueContainer
    
--------------------------------------------------------------------------

instance Mappeable (Ready2Val Portfolio) where    
    mapG modParams mktData vS (Ready2Val (Portfolio prs)) = do
        let rvprs = fmap (Ready2Val) prs
        grC      <- checkAllOk $ fmap fmapMapG (zip (subValues vS) rvprs)
        return (groupC $ Mo.mconcat grC)
        where 
             fmapMapG tupla = mapG modParams mktData (fst tupla) (snd tupla)   
    mapV vS (Ready2Val (Portfolio prs)) = do
        let rvprs = fmap ( Ready2Val) prs
        vC      <- checkAllOk $ fmap fmapMapV (zip (subValues vS) rvprs)
        return (Mo.mconcat vC)
        where 
             fmapMapV tupla = mapV (fst tupla) (snd tupla) 
--------------------------------------------------------------------------
 
instance Mappeable (Ready2Val Product) where
    mapG modParams mktData vS (Ready2Val (Swap l1 l2 af)) = do
        grC1 <- mapG modParams mktData ((subValues vS)!!0) (Ready2Val (l1))
        grC2 <- mapG modParams mktData ((subValues vS)!!1) (Ready2Val (l2))
        return (groupC $ Mo.mappend grC1 grC2)
    mapG modParams mktData vS (Ready2Val (Option l af)) = do
        grC1 <- mapG modParams mktData ((subValues vS)!!0) (Ready2Val (l))
        return (groupC grC1)        
    mapG modParams mktData vS (Ready2Val sw@(Swaption {})) 
        = Ok (groupC $ mapGreeksSwptn modParams mktData sw vS)      
    mapG modParams mktData vS x 
        = Error "There are not semi-analytical greeks for this product."
    
    mapV    vS (Ready2Val (Swap l1 l2 af))    = do
        return (ValueSwap 
                    (value vS) 
                    (value $ (subValues vS)!!0) 
                    (value $ (subValues vS)!!1) 
                    (fmap value (subValues ((subValues vS)!!0))) 
                    (fmap value (subValues ((subValues vS)!!1)))
               )
    mapV    vS (Ready2Val (Option l af))      = do
        return (ValueOption 
                    (value vS) 
                    (fmap value (subValues ((subValues vS)!!0))) 
               )
    mapV    vS              x                 = Error "Error showing result."       
--------------------------------------------------------------------------
        
instance Mappeable (Ready2Val Leg) where
    mapG modParams mktData vS (Ready2Val (l)) = do
        listCpnsCont <- checkAllOk $ fmap mapG' tuplaCsVs  
        return (groupC $ Mo.mconcat listCpnsCont)                                       
        where 
              mapG' tupla = mapG modParams mktData (fst tupla) (snd tupla)
              rVCoupons   = fmap Ready2Val (coupons l)
              tuplaCsVs   = zip (subValues vS) rVCoupons   
    mapV vS lr2v = do
        return (ValueLeg (value vS) (fmap value $ subValues vS))              
--------------------------------------------------------------------------
        
instance Mappeable (Ready2Val Coupon) where
    mapG modParams mktData vS (Ready2Val cp) = Ok (mapGreeks modParams mktData cp vS)
    mapV vS cr2v = do
        return (Value (value vS))    
--------------------------------------------------------------------------
instance Mo.Monoid GreeksContainer where  
    mempty = GreeksContainer Mo.mempty Mo.mempty
    mappend (GreeksContainer mk1 md1) (GreeksContainer mk2 md2) 
           = GreeksContainer (Mo.mappend mk1 mk2) (Mo.mappend md1 md2)
--------------------------------------------------------------------------
instance Mo.Monoid MarketContainer where  
    mempty = MarketContainer [] [] []
    mappend (MarketContainer rcc1 vcfc1 vswc1 ) (MarketContainer rcc2 vcfc2 vswc2 ) = 
        MarketContainer (groupRCC (rcc1 ++ rcc2)) 
                        (groupCFVC (vcfc1 ++ vcfc2)) 
                        (groupSWVC (vswc1 ++ vswc2))
        where
              groupRCC :: [RateCurveContainer] -> [RateCurveContainer]
              groupRCC    lsRCC                 = fmap group (nCurves lsRCC)
                  where 
                        nCurves lsrcc = nub $ fmap gCurveName lsrcc
                        group name = RateCurveContainer {gCurveValues = concat $ fmap gCurveValues 
                                                                                      sameCurve, 
                                                         gCurveName   = name}
                            where
                                sameCurve = filter (\rcc -> name == gCurveName rcc) lsRCC
              groupCFVC :: [CapFloorVolContainer] -> [CapFloorVolContainer]
              groupCFVC    lsCFVC                  = fmap group (iCFVols lsCFVC)
                  where 
                        iCFVols lscfv = nub $ fmap gCFIndex lscfv
                        group index   = CapFloorVolContainer {gCFValues = concat $ fmap gCFValues sameCFV, 
                                                              gCFIndex  = index}
                            where
                                sameCFV = filter (\cfvc -> index == gCFIndex cfvc) lsCFVC
              groupSWVC :: [SwaptionVolContainer] -> [SwaptionVolContainer]
              groupSWVC    lsSWVC                  = fmap group (cSWVols lsSWVC)
                  where 
                        cSWVols lsswv = nub $ fmap gSwCurr lsswv
                        group curr    = SwaptionVolContainer {gSwValues = concat $ fmap gSwValues sameSWV, 
                                                              gSwCurr  = curr}
                            where
                                sameSWV = filter (\swvc -> curr == gSwCurr swvc) lsSWVC
--------------------------------------------------------------------------
instance Mo.Monoid ModelParamsContainer where  
    mempty = ModelParamsContainer [] 
    mappend (ModelParamsContainer pc1) (ModelParamsContainer pc2 ) = 
        ModelParamsContainer (pc1 ++ pc2) 


--------------------------------------------------------------------------
mapGreeks :: ModelParameters -> MarketData -> Coupon -> ValueStorage 
          -> GreeksContainer
mapGreeks    mp                 md            cp        vs 
          = GreeksContainer (mapMktGreeks md cp vs) (mapModParamsGreeks mp cp vs)
--------------------------------------------------------------------------
mapGreeksSwptn :: ModelParameters -> MarketData -> Product -> ValueStorage 
               -> GreeksContainer
mapGreeksSwptn    mp                 md            swn        vs 
                = GreeksContainer (mapMktGreeksSwptn md swn vs) 
                                  (mapModParamsGreeksSwptn mp swn vs)
--------------------------------------------------------------------------
mapMktGreeks :: MarketData -> Coupon -> ValueStorage 
             -> MarketContainer
mapMktGreeks    md            cp        vs 
             = MarketContainer (mapRateCurveGreeks (curves md) cp vs) 
                               (mapCFVolGreeks cp vs) 
                               (mapSwVolGreeks cp vs) 
--------------------------------------------------------------------------
--------------------------------------------------------------------------
mapMktGreeksSwptn :: MarketData -> Product -> ValueStorage 
                  -> MarketContainer
mapMktGreeksSwptn    md            sw        vs 
                   = MarketContainer (mapRateCurveGreeksSwptn (curves md) sw vs)
                                     []
                                     (mapSwVolGreeksSwptn sw vs) 
--------------------------------------------------------------------------
mapRateCurveGreeks :: [RateCurve] -> Coupon -> ValueStorage 
                   -> [RateCurveContainer]
mapRateCurveGreeks    cvs
                      Fixed {cpDiscCurve = dc, cpPayDate = pd}                      
                      vs                                           
                      = fmap (allocateRCC cvs) 
                             [RateCurveContainer {gCurveName = dc,  
                                                  gCurveValues = [(pd, (greeks vs) !! 0)]}]
                              
--------------------------------------------------------------------------
mapRateCurveGreeks    cvs
                      Variable {cpDiscCurve = dc, cpIndex = i,
                      cpPayDate = pd, cpEstCurve = ec}                      
                      vs                                            
                      = fmap (allocateRCC cvs) 
                             [RateCurveContainer {gCurveName = dc, 
                                                  gCurveValues = [(pd, (greeks vs) !! 0)]},
                              RateCurveContainer {gCurveName = ec,   
                                                  gCurveValues = [(pd, (greeks vs) !! 0)]}]
                                            
--------------------------------------------------------------------------
mapRateCurveGreeksSwptn :: [RateCurve] -> Product -> ValueStorage 
                        -> [RateCurveContainer]
mapRateCurveGreeksSwptn    cvs sw vs                                           
                         = []
                              
                                            
--------------------------------------------------------------------------
mapCFVolGreeks :: Coupon -> ValueStorage -> [CapFloorVolContainer]

mapCFVolGreeks    Variable {cpDiscCurve = dc, cpIndex = i,
                  cpPayDate = pd, 
                  varPayOff = Libor fx st nd py cn mr,
                  varModel = ForwardNonStandard rD e t2P f v}   
                  vs                                            
                  = [CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx,fst4 f, (greeks vs) !! 2)]}]
--------------------------------------------------------------------------

mapCFVolGreeks    Variable {cpDiscCurve = dc, cpIndex = i,
                  cpPayDate = pd, 
                  varPayOff = Caplet fx st nd py cn mr k,
                  varModel = Black rD e v f}                     
                  vs                                            
                  = [CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx, k, (greeks vs) !! 2)]}]
--------------------------------------------------------------------------

mapCFVolGreeks    Variable {cpDiscCurve = dc, cpIndex = i,
                  cpPayDate = pd, 
                  varPayOff = Floorlet fx st nd py cn mr k,
                  varModel = Black rD e v f}                     
                  vs                                            
                  = [CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx, k,(greeks vs) !! 2)]}]
--------------------------------------------------------------------------

mapCFVolGreeks    Variable {cpDiscCurve = dc, cpIndex = i,
                  cpPayDate = pd, 
                  varPayOff = Caplet fx st nd py cn mr k,
                  varModel = BlackNonStandard rD e t2P v f vAd}  
                  vs                                            
                  = [CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx, k, (greeks vs) !! 2)]},
                     CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx,fst4 f, (greeks vs) !! 3)]}]
--------------------------------------------------------------------------

mapCFVolGreeks    Variable {cpDiscCurve = dc, cpIndex = i,
                  cpPayDate = pd, 
                  varPayOff = Floorlet fx st nd py cn mr k,
                  varModel = BlackNonStandard rD e t2P v f vAd}                     
                  vs                                            
                  = [CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx, k, (greeks vs) !! 2)]},
                     CapFloorVolContainer {gCFIndex = i, gCFValues = [(fx,fst4 f, (greeks vs) !! 3)]}]
--------------------------------------------------------------------------
mapCFVolGreeks    cp        vs                                  = []
--------------------------------------------------------------------------

mapSwVolGreeks :: Coupon -> ValueStorage -> [SwaptionVolContainer]
mapSwVolGreeks    cp        vs            = []
--------------------------------------------------------------------------

mapSwVolGreeksSwptn :: Product -> ValueStorage -> [SwaptionVolContainer]
mapSwVolGreeksSwptn    cp        vs            = []
--------------------------------------------------------------------------

mapModParamsGreeks :: ModelParameters -> Coupon -> ValueStorage 
                   -> ModelParamsContainer
mapModParamsGreeks    mp                 cp        vs            
                    = ModelParamsContainer []        
--------------------------------------------------------------------------

mapModParamsGreeksSwptn :: ModelParameters -> Product -> ValueStorage 
                        -> ModelParamsContainer
mapModParamsGreeksSwptn    mp                 cp         vs            
                         = ModelParamsContainer []        
  
--------------------------------------------------------------------------
instance Mo.Monoid ValueContainer where  
    mempty          = Value 0.0 
    mappend vC1 vC2 = Value ((valueCont vC1) + (valueCont vC2))
    
--------------------------------------------------------------------------
instance Mo.Monoid Viewer where  
    mempty          = Viewer Mo.mempty Mo.mempty 
    mappend vV1 vV2 = Viewer (Mo.mappend (valueResult vV1)  (valueResult vV2)) 
                             (Mo.mappend (greeksResult vV1) (greeksResult vV2))  
        

--------------------------------------------------------------------------
allocateRCC :: [RateCurve] -> RateCurveContainer -> RateCurveContainer
allocateRCC    cvs            rcc                 
             = RateCurveContainer {gCurveName = ncv, 
                                   gCurveValues = concat $ fmap (allocateRCV cv) 
                                                                (gCurveValues rcc)}
    where
          ncv = gCurveName rcc
          cv  = (filter (\cv -> curveName cv == ncv) cvs) !! 0

--------------------------------------------------------------------------
allocateRCV :: RateCurve -> Container1 -> [Container1]
allocateRCV    cv           c                 
             = listVals
    where
          pillar1  = if (fst c) <= (minimum $ pillarMaturities cv)  
                     then (head $ pillarMaturities cv)
                     else let nLess = (length $ filter (< fst c) 
                                                       (pillarMaturities cv))
                          in (pillarMaturities cv) !! (nLess - 1)
          pillar2  = if (fst c) >= (maximum $ pillarMaturities cv)  
                     then (last $ pillarMaturities cv)
                     else let nLess = (length $ filter (< fst c) 
                                                       (pillarMaturities cv))
                          in (pillarMaturities cv) !! nLess
          value1   = 0.5 * (snd c)
          value2   = 0.5 * (snd c)
          listVals = [(pillar1, value1), (pillar2, value2)]   
--------------------------------------------------------------------------
            
groupGC :: GreeksContainer -> GreeksContainer
groupGC    gc               = gc   

  
--------------------------------------------------------------------------
-- Groupable class
class Groupable m where
    groupC :: m -> m 
--------------------------------------------------------------------------

instance Groupable GreeksContainer where  
    groupC gc = GreeksContainer (groupC $ gMktGreeks gc) 
                                (groupC $ gModParamsGreeks gc)
        
--------------------------------------------------------------------------
        
instance Groupable MarketContainer where  
    groupC mc = MarketContainer (groupC $ gCurves mc) 
                                (groupC $ gCapFloorVols mc)
                                (groupC $ gSwaptionVols mc)

--------------------------------------------------------------------------
instance Groupable ModelParamsContainer where  
    groupC mp = ModelParamsContainer (groupC $ gParams mp)                                            
                          
--------------------------------------------------------------------------
instance Groupable [RateCurveContainer] where  
    groupC rccs = fmap groupC (groupC' cnames)
        where  
              -- Group first by curve
              groupC' cnames = fmap (groupAll . fbyname) cnames
              fbyname cname  = filter (\rcc -> gCurveName rcc == cname) rccs
              cnames         = nub $ fmap gCurveName rccs
              groupAll rccs  = RateCurveContainer (gCurveName (rccs!!0))
                                                  (concat $ fmap gCurveValues rccs)
--------------------------------------------------------------------------
instance Groupable RateCurveContainer where  
    groupC rcc = RateCurveContainer (gCurveName rcc) (groupC $ gCurveValues rcc)
--------------------------------------------------------------------------
instance Groupable [Container1] where  
    groupC cs1 = fmap (\d -> (d, sumPerDay d)) days
        where 
              sumPerDay day = sum $ fmap snd (filter (\(d,v) -> d == day) cs1)
              days          = nub $ fmap fst cs1
--------------------------------------------------------------------------
instance Groupable [CapFloorVolContainer] where  
    groupC cfvcs = fmap groupC (groupC' cfvindexs) 
        where  
              -- Group first by index
              groupC' cfvindexs = fmap (groupAll . fbyindex) cfvindexs
              fbyindex cfvindex = filter (\cfvc -> gCFIndex cfvc == cfvindex) cfvcs
              cfvindexs         = nub $ fmap gCFIndex cfvcs
              groupAll cfvcs  = CapFloorVolContainer (gCFIndex (cfvcs!!0))
                                                     (concat $ fmap gCFValues cfvcs)
--------------------------------------------------------------------------
instance Groupable CapFloorVolContainer where  
    groupC cfvc = CapFloorVolContainer (gCFIndex cfvc) 
                                       (groupC $ gCFValues cfvc)
--------------------------------------------------------------------------
instance Groupable [Container2] where  
    groupC cs2 = fmap (\t@(e, s) -> (e, s, sumPerExpStr t)) expstrs
        where 
              sumPerExpStr expstr = sum $ fmap trd3 
                                               (filter (\(d, str, v) 
                                                            -> (d, str) == expstr) 
                                                cs2)
              expstrs             = nub $ fmap (\(d, str, v) -> (d, str)) cs2
--------------------------------------------------------------------------
instance Groupable [SwaptionVolContainer] where  
    groupC svcs = fmap groupC (groupC' svcurrs) 
        where  
              -- Group first by currency
              groupC' svcurrs = fmap (groupAll . fbycurr) svcurrs
              fbycurr svcurr  = filter (\svc -> gSwCurr svc == svcurr) svcs
              svcurrs         = nub $ fmap gSwCurr svcs
              groupAll svcs   = SwaptionVolContainer (gSwCurr (svcs!!0))
                                                     (concat $ fmap gSwValues svcs)
--------------------------------------------------------------------------
instance Groupable SwaptionVolContainer where  
    groupC swvc = SwaptionVolContainer (gSwCurr swvc) 
                                       (groupC $ gSwValues swvc)
--------------------------------------------------------------------------
instance Groupable [Container3] where  
    groupC cs3 = fmap (\tu@(e, t, s) -> (e, t, s, sumPerETStr tu)) etstrs
        where 
              sumPerETStr etstr = sum $ fmap fth4 
                                               (filter (\(e, t, str, v) 
                                                            -> (e, t, str) == etstr) 
                                                       cs3)
              etstrs            = nub $ fmap (\(e, t, str, v) -> (e, t, str)) cs3
--------------------------------------------------------------------------
instance Groupable [ParamsContainer] where  
    groupC pcs = pcs       
--------------------------------------------------------------------------                              
                          
                          
