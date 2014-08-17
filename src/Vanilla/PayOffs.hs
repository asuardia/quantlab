{-# LANGUAGE DeriveDataTypeable #-}
module Vanilla.PayOffs   
    ( 
     PayOff (..)
    ) where 

import Data.Time.Calendar
import Utils.MyJSON
import Market.FinantialConventions

-- Pay Offs
data PayOff = Libor       {
                               liborFix :: Day,
                               liborStart :: Day, 
                               liborEnd :: Day, 
                               liborPay :: Day, 
                               liborConvention :: Convention, 
                               margin :: Double
                          } 
            | Caplet      {
                               liborFix :: Day, 
                               liborStart :: Day, 
                               liborEnd :: Day, 
                               liborPay :: Day, 
                               liborConvention :: Convention, 
                               margin :: Double, 
                               capStrike :: Double
                          }  
            | Floorlet    {    liborFix :: Day, 
                               liborStart :: Day, 
                               liborEnd :: Day, 
                               liborPay :: Day, 
                               liborConvention :: Convention, 
                               margin :: Double, 
                               floorStrike :: Double}  
            | CMS         {    cmsFix :: Day, 
                               cmsDates :: [Day], 
                               cmsConvention :: Convention, 
                               cmsMargin :: Double
                          }  
            | CapletCMS   {    cmsFix :: Day, 
                               cmsDates :: [Day], 
                               cmsConvention :: Convention, 
                               cmsMargin :: Double, 
                               capStrike :: Double}  
            | FloorletCMS {    cmsFix :: Day, 
                               cmsDates :: [Day], 
                               cmsConvention :: Convention, 
                               cmsMargin :: Double, 
                               floorStrike :: Double
                          } deriving (Eq, Show, Data, Typeable)  
