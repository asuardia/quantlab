module Math.Interpolation   
    ( 
     interp1
    ) where
    
interp1 :: [Double] -> [Double] -> Double -> Double
interp1    x           y           x0      = y0
    where 
          y0 = 0.9
