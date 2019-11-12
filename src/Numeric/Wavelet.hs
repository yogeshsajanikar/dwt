module Numeric.Wavelet where


-- A wavelet filter takes a pair and generates either an approximation or a detail.
type Filter a = a -> a -> a


data Wavelet a = Wavelet { low :: Filter a, high :: Filter a, scale :: a }


lowPass :: (Fractional a, Num a) => a -> a -> a
lowPass x y = (x + y )

highPass :: (Fractional a, Num a) => a -> a -> a
highPass x y = (y - x)

transform :: Fractional a => Wavelet a -> [a] -> [a]
transform w xs =
  transform' xs [] []
  where
    lowF = low w
    highF = high w
    s = scale w
    
    transform' []       (a:[])  details = a:details
    transform' []       approxs details = transform' approxs [] details
    transform' (x:[])   approxs details = transform' approxs [] details
    transform' (x:y:xs) approxs details =
      let l = s * lowF x y
          h = s * highF x y
      in transform' xs (l:approxs) (h:details)

