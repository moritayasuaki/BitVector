{-# LANGUAGE TypeSynonymInstances,OverlappingInstances #-}

module SBV where

import Data.Array.Unboxed
import Data.Bits
import Data.Word

type W64 = Word64
type BV = UArray W64 W64

(.>>.) :: (Bits a, Integral b) => a -> b -> a
(.>>.) = \w i -> w `unsafeShiftR` fromIntegral i
{-# INLINE (.>>.) #-}

(.<<.) :: (Bits a, Integral b) => a -> b -> a
(.<<.) = \w i -> w `unsafeShiftL` fromIntegral i
{-# INLINE (.<<.) #-}


chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = take n xs : chunks n (drop n xs)

-- |
-- >>> showBits 0x8
-- "0001000000000000000000000000000000000000000000000000000000000000"
showBits :: W64 -> String
showBits w = [ t ((w .>>. i) .&. 1) | i <- [0..63]]
         where t 0 = '0'
               t _ = '1'

readBits :: String -> W64
readBits s = sum . zipWith (\i b -> b .<<. i ) [0..63] . map t' $ s 
         where t' '0' = 0
               t' _ = 1

-- |
-- >>> showBV $ array (0,1) [(0,0xFF),(1,0x01)]
-- "11111111000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000"
showBV :: BV -> String
showBV bv = concatMap (\i -> showBits (bv ! i) ) (indices bv)


-- |
-- >>> readBV "101011111011111111110000000000000000000000001111111111111111111111111111100000000000000000000011111111111"
-- array (0,1) [(0,18446726481524555253),(1,2197949514239)]
readBV :: String -> BV
readBV s = listArray (0,fromIntegral (length ws - 1)) . map readBits $ ws
    where ws = chunks 64 s

listUArray :: [W64] -> UArray W64 W64
listUArray ws = listArray (0,fromIntegral (length ws - 1)) ws

-- |
-- >>> popCnt $ readBits "1110011"
-- 5

popCnt :: W64 -> W64
popCnt = \x ->
           let x0 = x - ((x .&. 0xAAAAAAAAAAAAAAAA) .>>. 1)
               x1 = (x0 .&. 0x3333333333333333) + ((x0 .>>. 2) .&. 0x3333333333333333)
               x2 = (x1 + (x1 .>>. 4)) .&. 0x0F0F0F0F0F0F0F0F 
           in (x2 * 0x0101010101010101) .>>. 56
{-# INLINE popCnt #-}

m :: W64 -> W64 -> W64
m = \s w -> let t = (w .&. 7) - 1 in
              s .>>. (t + ((t .>>. 60) .&. 8)) * 9 .&. 0x1FF
{-# INLINE m #-}

-- raw bit array
-- <-                         8 words                            ->
-- <- 64 -><- 64 -><- 64 -><- 64 -><- 64 -><- 64 -><- 64 -><- 64 -> 
-- 01010...101001..1000....1001....001.....100.....000.....101.....

-- rank index
--
-- first level 
-- <-               64 bits unsigned integer           ->
--                number of cumulative popCount

-- <-                 7 block                     ->
-- <- 9 -><- 9 -><- 9 -><- 9 -><- 9 -><- 9 -><- 9 -><-1->
--  diff1  diff2  diff3  diff4  diff5  diff6  diff7 

-- 64 * 7 = 448 (maximum diff number)
-- it can be represented by 9 bits integer 

-- w : first level index (rank)
-- s : second level index
-- take 9 bits * 7 block = 63 bits

-- |
-- >>> let list = take 1000 $ cycle [0x0,0x1,0x3,0xF,0xFF,0xFFFF,0xFFFFFFFF,-1] :: [W64]
-- >>> let a = listUArray list
-- >>> let b = mkRank9 a
-- >>> showRank9 b

mkRank9 :: BV -> BV
mkRank9 bv = listUArray (encode blks 0)
    where pops = [popCnt (bv ! i) | i <- indices bv]
          blks = chunks 8 pops
          encode [] rank = []
          encode (x:xs) rank = 
              rank : fold (scanl1 (+) x) : encode xs (rank + sum x) 
          fold xs = sum $ zipWith (.<<.) xs [0,9..56]

showRank9 :: BV -> String
showRank9 bv = show [(bv ! l, unfold (bv ! s)) | [l,s] <- chunks 2 (indices bv)]
    where unfold x = map (\i -> (x .>>. i) .&. 0x1FF) [0,9..56]
