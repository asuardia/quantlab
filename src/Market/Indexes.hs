{-# LANGUAGE DeriveDataTypeable #-}
module Market.Indexes   
    ( 
     eurCMS2yCol, eurCMS2y,     eurCMS5y,  eurCMS10yCol, eurCMS10y, eurCMS15yCol, 
     eurCMS15y,   eurCMS20yCol, eurCMS20y, eurCMS30yCol, eurCMS30y, euribor1mCol, 
     euribor1m,   euribor3mCol, euribor3m, euribor6mCol, euribor6m, euribor12mCol, 
     euribor12m,  listIndex
    ) where

import Utils.MyJSON
import Market.FinantialConventions

-- Indexes
listIndex = [
             eurCMS2yCol, eurCMS2y,     eurCMS5y,  eurCMS10yCol, eurCMS10y, eurCMS15yCol, 
             eurCMS15y,   eurCMS20yCol, eurCMS20y, eurCMS30yCol, eurCMS30y, euribor1mCol, 
             euribor1m,   euribor3mCol, euribor3m, euribor6mCol, euribor6m, euribor12mCol, 
             euribor12m
            ]
             
eurCMS2yCol  = Index {
                         iName  = "EURCMS2Y-COL",                     
                         iDescription = "EURCMS2Y (Ope. colateralizadas)", 
                         iRateNaure  = SwapRate,
                         iTenor = Tenor {tUnits = 2, tPeriod = Year}, 
                         iStartDelay = OpenDays {nDays = 2}, 
                         iConvention = (LIN, THIRTY360),              
                         iScheduleGenerator = ModFol {sgUnits = 1, sgPeriod = Year}
                     }
eurCMS2y     = Index {
                         iName = "EURCMS2Y", 
                         iDescription = "EURCMS2Y", 
                         iRateNaure = SwapRate,
                         iTenor = Tenor {tUnits = 2, tPeriod = Year}, 
                         iStartDelay = OpenDays {nDays = 2}, 
                         iConvention = (LIN, THIRTY360), 
                         iScheduleGenerator = ModFol {sgUnits = 1, sgPeriod = Year}
                     }
eurCMS5yCol  = Index {iName = "EURCMS5Y-COL", iDescription = "EURCMS5Y (Ope. colateralizadas)", iRateNaure = SwapRate,
                   iTenor = Tenor {tUnits = 5, tPeriod = Year}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, THIRTY360), iScheduleGenerator = ModFol {sgUnits = 1, sgPeriod = Year}}
eurCMS5y     = Index {iName = "EURCMS5Y", iDescription = "EURCMS5Y", iRateNaure = SwapRate,
                   iTenor = Tenor {tUnits = 5, tPeriod = Year}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, THIRTY360), iScheduleGenerator = ModFol {sgUnits = 1, sgPeriod = Year}}
eurCMS10yCol = Index {iName = "EURCMS10Y-COL", iDescription = "EURCMS10Y (Ope. colateralizadas)", iRateNaure = SwapRate,
                   iTenor = Tenor {tUnits = 10, tPeriod = Year}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, THIRTY360), iScheduleGenerator = ModFol {sgUnits = 1, sgPeriod = Year}}
eurCMS10y    = Index {iName = "EURCMS10Y", iDescription = "EURCMS10Y", iRateNaure = SwapRate,
                   iTenor = Tenor {tUnits = 10, tPeriod = Year}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, THIRTY360), iScheduleGenerator = ModFol {sgUnits = 1, sgPeriod = Year}}
eurCMS15yCol = Index {iName = "EURCMS15Y-COL", iDescription = "EURCMS15Y (Ope. colateralizadas)", iRateNaure = SwapRate,
                   iTenor = Tenor {tUnits = 15, tPeriod = Year}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, THIRTY360), iScheduleGenerator = ModFol {sgUnits = 1, sgPeriod = Year}}
eurCMS15y    = Index {iName = "EURCMS15Y", iDescription = "EURCMS15Y", iRateNaure = SwapRate,
                   iTenor = Tenor {tUnits = 15, tPeriod = Year}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, THIRTY360), iScheduleGenerator = ModFol {sgUnits = 1, sgPeriod = Year}}
eurCMS20yCol = Index {iName = "EURCMS20Y-COL", iDescription = "EURCMS20Y (Ope. colateralizadas)", iRateNaure = SwapRate,
                   iTenor = Tenor {tUnits = 20, tPeriod = Year}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, THIRTY360), iScheduleGenerator = ModFol {sgUnits = 1, sgPeriod = Year}}
eurCMS20y    = Index {iName = "EURCMS20Y", iDescription = "EURCMS20Y", iRateNaure = SwapRate,
                   iTenor = Tenor {tUnits = 20, tPeriod = Year}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, THIRTY360), iScheduleGenerator = ModFol {sgUnits = 1, sgPeriod = Year}}
eurCMS30yCol = Index {iName = "EURCMS30Y-COL", iDescription = "EURCMS30Y (Ope. colateralizadas)", iRateNaure = SwapRate,
                   iTenor = Tenor {tUnits = 30, tPeriod = Year}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, THIRTY360), iScheduleGenerator = ModFol {sgUnits = 1, sgPeriod = Year}}
eurCMS30y    = Index {iName = "EURCMS30Y", iDescription = "EURCMS30Y", iRateNaure = SwapRate,
                   iTenor = Tenor {tUnits = 30, tPeriod = Year}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, THIRTY360), iScheduleGenerator = ModFol {sgUnits = 1, sgPeriod = Year}}
euribor1mCol = Index {iName = "EURIBOR1M-COL", iDescription = "EURIBOR1M (Ope. colateralizadas)", iRateNaure = Standard,
                   iTenor = Tenor {tUnits = 1, tPeriod = Month}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, ACT360), iScheduleGenerator = ModFol {sgUnits = 6, sgPeriod = Month}}
euribor1m    = Index {iName = "EURIBOR1M", iDescription = "EURIBOR1M", iRateNaure = Standard,
                   iTenor = Tenor {tUnits = 1, tPeriod = Month}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, ACT360), iScheduleGenerator = ModFol {sgUnits = 6, sgPeriod = Month}}
euribor3mCol = Index {iName = "EURIBOR3M-COL", iDescription = "EURIBOR3M (Ope. colateralizadas)", iRateNaure = Standard,
                   iTenor = Tenor {tUnits = 3, tPeriod = Month}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, ACT360), iScheduleGenerator = ModFol {sgUnits = 6, sgPeriod = Month}}
euribor3m    = Index {iName = "EURIBOR3M", iDescription = "EURIBOR3M", iRateNaure = Standard,
                   iTenor = Tenor {tUnits = 3, tPeriod = Month}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, ACT360), iScheduleGenerator = ModFol {sgUnits = 6, sgPeriod = Month}}
euribor6mCol = Index {iName = "EURIBOR6M-COL", iDescription = "EURIBOR6M (Ope. colateralizadas)", iRateNaure = Standard,
                   iTenor = Tenor {tUnits = 6, tPeriod = Month}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, ACT360), iScheduleGenerator = ModFol {sgUnits = 6, sgPeriod = Month}}
euribor6m    = Index {iName = "EURIBOR6M", iDescription = "EURIBOR6M", iRateNaure = Standard,
                   iTenor = Tenor {tUnits = 6, tPeriod = Month}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, ACT360), iScheduleGenerator = ModFol {sgUnits = 6, sgPeriod = Month}}
euribor12mCol = Index {iName = "EURIBOR12M-COL", iDescription = "EURIBOR12M (Ope. colateralizadas)", iRateNaure = Standard,
                   iTenor = Tenor {tUnits = 12, tPeriod = Month}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, ACT360), iScheduleGenerator = ModFol {sgUnits = 6, sgPeriod = Month}}
euribor12m = Index {iName = "EURIBOR12M", iDescription = "EURIBOR12M", iRateNaure = Standard,
                   iTenor = Tenor {tUnits = 12, tPeriod = Month}, iStartDelay = OpenDays {nDays = 2}, 
                   iConvention = (LIN, ACT360), iScheduleGenerator = ModFol {sgUnits = 6, sgPeriod = Month}}