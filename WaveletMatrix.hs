module WaveletMatrix where

import Data.List
import Data.Array.Unboxed
import Data.Word
import Data.Bits
import Control.Applicative

testbit :: Int -> Word8 -> Bool
testbit n w = (w .&. (1 `unsafeShiftL` n)) /= 0

test :: Int -> Word8 -> Word8
test n w = (w .&. (1 `unsafeShiftL` n))

-- |
-- >>> toWM [0x02,0x3f,0x1F]
toWM :: [Word8] -> [([Word8],Int)]
toWM str = (f 0 str)
  where f 8 _ = []
        f n str = (map (test n) str,length zeros) : f (n+1) (zeros++ones)
            where (zeros,ones) = gr n str
        gr n str = (filter (not . testbit n) str, filter (testbit n) str)


