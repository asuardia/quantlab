module Math.Interpolation   
    ( 
     interp1,
     interp2
    ) where
    
interp1 :: [Double] -> [Double] -> Double -> Double
interp1    x           y           x0      = y0
    where 
          y0 = 0.9

interp2 :: [Double] -> [Double] -> [[Double]] -> Double -> Double -> Double
interp2    x           y           z             x0        y0      = z0
    where 
          z0 = 0.2