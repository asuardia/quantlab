
module Utils.MyUtils   
    ( 
     split, flip123_312, eqtol
    ) where
    
    
-------------------- MyUtils ---------------------------------------------
--------------------------------------------------------------------------    
split :: String -> Char  -> [String]
split    []        delim =  [""]
split    (c:cs)    delim
   | c == delim          = "" : rest
   | otherwise           = (c : head rest) : tail rest
   where
         rest = split cs delim    
--------------------------------------------------------------------------
flip123_312 :: (a -> b -> c -> d) -> c -> a -> b -> d 
flip123_312    f                     z    x    y  = f x y z 
--------------------------------------------------------------------------
eqtol tol a b = tol > abs (a-b)
