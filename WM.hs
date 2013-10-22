{-# LANGUAGE TypeFamilies,FlexibleInstances,FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables,MultiParamTypeClasses #-}

import Control.Applicative
import Control.Lens
import Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed as V
import Data.Vector.Lens
import Data.Bits
import Data.Bits.Lens
import Data.Binary
import Data.Word
import Prelude as P
import Data.List as L

type N = Int

bitW :: Int
bitW = bitSize (-1::Int)

-- |
-- A[0..k)
-- rank(v,q,i) = |{ j | A[j] = q, j ∈ [0..i) }|
-- access(v,q,i) = min { j | A[j] = q, j ∈ [0..i) }

class RSV v where
    type E v
    size :: v -> N
    access :: v -> N -> E v
    rank :: v -> E v -> N -> N
    select :: v -> E v -> N -> N

instance Eq a => RSV [a] where
    type E [a] = a
    size = P.length
    access = (!!)
    rank xs e n = size . P.filter (== e) $ P.take n xs
    select xs e n =  fst (P.filter (\p -> snd p == e) (P.zip [0..] xs) !! n)

newtype BInt = BInt { btoi :: Int }
instance RSV BInt where
    type E BInt = Bool
    size (BInt b) = bitW
    access (BInt b) i = (b .&. (1 `unsafeShiftL` i)) == 1
    rank = rankInt . btoi
    select = selectInt . btoi


-- |
-- >>> rank (fromList [0x1F,0x30] :: Vector Int) True 69
-- 6

-- |
-- >>> select (fromList [0x1F,0x30] :: Vector Int) True 6
-- 69

instance RSV (Vector Int) where
    type E (Vector Int) = Bool
    size = V.length
    access v i = testBit ofs (v ! ind) 
        where (ind,ofs) = i `divMod` bitW 
    rank v True i = ofscnt + V.sum (V.map popCount (V.unsafeSlice 0 ind v))
        where ofscnt = rankInt (v ! ind) True ofs
              (ind,ofs) = i `divMod` bitW
    rank v False i = i - rank v True i
    select v True i = bitW * ind + ofs
        where vp = V.scanl' (+) 0 (V.map popCount v)
              vf = V.takeWhile (< i) vp
              ind = V.length vf - 1
              ofs = selectInt (v!ind) True (i-vp!ind)

-- |
-- >>> rank ([1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0]::[Int]) 1 9
-- 2

-- |
-- >>> select ([1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0]::[Int]) 1 2
-- 9

-- |
-- >>> rankInt 0x1F True 5
-- 5

-- |
-- >>> selectInt 0x1F True 5
-- 5

rankInt :: Int -> Bool -> Int -> Int
rankInt x True n = popCount (x .&. ((unsafeShiftL 1 n) - 1))
rankInt x False n = bitW - rankInt x True n

selectInt :: Int -> Bool -> Int -> Int
selectInt x b n = ans
    where f = if b then 0 else 1
          cumsum :: [Int]
          cumsum = P.scanl (+) 0 [f `xor` ((x `unsafeShiftR` s) .&. 1) | s <- [0..bitW-1]]
          Just ans = L.findIndex (== n) cumsum

popCumSum8 :: Int -> Int
popCumSum8 s1 = let s2 = (s1 .&. 0x5555555555555555) +
                         ((s1 .&. 0xAAAAAAAAAAAAAAAA) `unsafeShiftR` 1)
                    s4 = (s2 .&. 0x3333333333333333) + 
                         ((s2 .&. 0xCCCCCCCCCCCCCCCC) `unsafeShiftR` 2)
                    s8 = (s4 .&. 0x0F0F0F0F0F0F0F0F) +
                         ((s4 .&. 0xF0F0F0F0F0F0F0F0) `unsafeShiftR` 4)
                    cs8 = s8 * 0x0101010101010101
                 in cs8

top8 :: Int -> Int
top8 x = x `unsafeShiftR` (bitW - 8)

-- |
-- >>> popCount' 0x0103070F
-- 10
popCount' :: Int -> Int
popCount' = top8 . popCumSum8


main = pure ()

