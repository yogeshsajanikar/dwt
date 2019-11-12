module Numeric.Wavelet.Dyadic where


-- For a given integer m, find out j such that 2^(j-1) < m =< 2^j if j > 1 else
-- 0 < m <= 2 otherwise
dyadicRange :: Integral a => a -> a
dyadicRange n | n <= 0 = 0
dyadicRange n = ceiling $ logBase 2 $ fromIntegral n


dyadicInterval j k logN | j > logN || j < 1 || k > 2^j-1 || k < 0 = Nothing
dyadicInterval j k logN = Just (kFactor, kFactor + kLevel -1)
  where
    kLevel = 2^(logN -j)
    kFactor = k * kLevel


-- Given a range [a, b], and a subset [c, d], return a vector, v  [1 ...(b-a)] such that
-- v(i) = 1 if i \elementOf [c,d] else 0
rangeVector (a, b) (c, d) | b < a || d < c || c > b || c < a || c > b || d < a || d > b = []
rangeVector (a, b) (c, d) = [ inRange x c d | x <- [a..b] ]
  where
    inRange x c d | x >= c && x <= d = 1
    inRange _ _ _  = 0


-- Given a level j, level index k, create a vector of size n, note
-- that the dyadicRange of n must be logN
dyadicVector :: (Integral a, Num a) => a -> a -> a -> a -> [a]
dyadicVector j k logN n | dyadicRange n /= logN = []
dyadicVector j k logN n =
   case dyadicInterval j k logN of
     Nothing     -> []
     Just (a, b) -> take (fromIntegral n) $ rangeVector (0, 2^logN -1) (a, b)

waveletVector :: (Integral a, Num a) => a -> a -> a -> a -> [a]
waveletVector j k logN n | dyadicRange n /= logN = []
waveletVector j k logN n | j < 0 && j >= logN    = []
waveletVector j k logN n | k < 0 && k >= 2^logN  = []
waveletVector j k logN n =
  let phi = dyadicVector (j + 1) (2*k) logN n
      phi_1 = dyadicVector (j + 1) (2*k + 1) logN n
  in zipWith (\p p1 -> -p + p1) phi phi_1
      

scaleFactor :: (Integral a, Num a, Floating b, Num b) => a -> a -> b
scaleFactor j logN =
  let p = (fromIntegral logN - fromIntegral j) * 0.5
  in (fromIntegral 2) ** (p + fromIntegral j - fromIntegral logN )

-- waveletCoefficient :: (Integral a, Num a, Fractional a, Num b) => a -> a -> a -> a -> [b]
waveletCoefficient j k logN n =
  let s = scaleFactor j logN
  in (\x -> fromIntegral x * s) <$> waveletVector j k logN n

averageCoefficient logN n =
  take n $ repeat 1

-- dydadic intervals
dydadicIntervals n | n < 1 = []
dydadicIntervals n =
  let dydadicInterval j kLevel k =
        let kFactor = k*kLevel
        in (kFactor, kFactor + kLevel - 1)

      kDydadicIntervals j =
        let kUpper = 2^j
        in dydadicInterval j (2^(n-j)) <$> [0..(kUpper-1)]
            
  in kDydadicIntervals <$> [1..n]


-- Spits out dyadic sequence of (j, k) where 
dyadicSequence logN =
  let jk = (\x -> (x, 2^x-1)) <$> [0..(logN-1)]
  in generateSequence jk [] 0 
  where
    generateSequence []              xs _ = reverse xs
    generateSequence ((j, k) : jks)  xs n | k < 0 = generateSequence jks xs n
    generateSequence ((j, k) : jks)  xs n =
      case xs of
        []              -> generateSequence ((j, k-1) : jks) [(j, 0, n)]  (n + 1)
        (j', k', _):_   -> if j' == j
          then generateSequence ((j, k-1) : jks) ((j, k'+1, n):xs) (n + 1)
          else generateSequence ((j, k-1) : jks) ((j,    0, n):xs) (n + 1)
          

dotProduct xs ys = sum $ zipWith (*) xs ys
