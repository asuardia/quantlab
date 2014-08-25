module Math.Interpolation   
    ( 
     interp1, interp2
    ) where
    
    
-------------------------------------------------------------------------- 
interp1 :: [Double] -> [Double] -> Double -> Double
interp1    x           y           x0      
    | x0 <= (minimum x) = head y
    | x0 >= (maximum x) = last y
    | otherwise = y0
    where 
          less = filter (< x0) x
          xD   = last less
          xU   = x !! (length less)
          yD   = y !! ((length less) - 1)
          yU   = y !! (length less)
          y0   = yD + ((yU - yD) / (xU - xD)) * (x0 - xD)

-------------------------------------------------------------------------- 
interp2 :: [Double] -> [Double] -> [[Double]] -> Double -> Double -> Double
interp2    x           y           z             x0        y0      = z0
    where 
          z0 = 0.2