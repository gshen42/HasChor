-- Karatsuba Fast Multiplication Reference Implementation
import System.Environment

karatsuba :: Integer -> Integer -> Integer
karatsuba n1 n2 =
  if n1 < 10 || n2 < 10
    then n1 * n2
    else result
  where
    log10 :: Integer -> Double
    log10 = logBase 10 . fromIntegral
    m = (max (log10 n1) (log10 n2)) + 1
    m2 = floor (m / 2)
    splitter = 10 ^ m2
    h1 = n1 `div` splitter
    l1 = n1 `mod` splitter
    h2 = n2 `div` splitter
    l2 = n2 `mod` splitter
    z0 = karatsuba l1 l2
    z2 = karatsuba h1 h2
    z1 = (karatsuba (l1 + h1) (l2 + h2)) - z2 - z0
    result = z2 * splitter * splitter + z1 * splitter + z0

main :: IO ()
main = do
  [n1, n2] <- map read <$> getArgs
  print (karatsuba n1 n2)
