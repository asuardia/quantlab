{-# LANGUAGE DeriveDataTypeable #-}
module Market.Currencies   
    ( 
     Currency (..)
    ) where
    
import Utils.MyJSON

-- Some finantial conventions
data Currency = EUR 
              | USD 
              | GBP deriving (Eq, Show, Data, Typeable, Read)
