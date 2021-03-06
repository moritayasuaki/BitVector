{-# LANGUAGE TypeFamilies,FlexibleInstances,FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables,MultiParamTypeClasses #-}

import Control.Applicative
import Control.Lens
import Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed as V hiding (mempty)
import Data.Vector.Lens
import Data.Bits
import Data.Bits.Lens
import Data.Binary
import Data.Word
import Prelude as P
import Data.List as L
import Data.Monoid as Mon
import Numeric

type N = Int

bitW :: Int
bitW = bitSize (-1::Int)

(.>>.) :: Int -> Int -> Int
x .>>. n = fromIntegral (fromIntegral x `unsafeShiftR` fromIntegral n :: Word64)

(.<<.) :: Int -> Int -> Int
x .<<. n = fromIntegral (fromIntegral x `unsafeShiftL` fromIntegral n :: Word64)

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
    access (BInt b) i = (b .&. (1 .<<. i)) == 1
    rank = rankInt . btoi
    select = selectInt . btoi


-- |
-- >>> rank (fromList [0x1F,0x30] :: Vector Int) True 64
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

-- |
-- >>> rankInt (-1) True 3
-- 3

rankInt :: Int -> Bool -> Int -> Int
rankInt x True n = popCount (x .&. m)
    where m = complement 0 .>>. (64 - n)

rankInt x False n = n - rankInt x True n

selectInt :: Int -> Bool -> Int -> Int
selectInt x b n = ans
    where f = if b then 0 else 1
          cumsum :: [Int]
          cumsum = P.scanl (+) 0 [f `xor` ((x .>>. s) .&. 1) | s <- [0..bitW-1]]
          Just ans = L.findIndex (== n) cumsum

selectInt' :: Int -> Bool -> Int -> Int
selectInt' s1 True n = test2
    where s2  = (  s1 .&. 0x5555555555555555) +
                (( s1 .&. 0xAAAAAAAAAAAAAAAA) .>>. 1) -- (2-2) * 32 = 0 space
          s4  = (  s2 .&. 0x3333333333333333) + 
                (( s2 .&. 0xCCCCCCCCCCCCCCCC) .>>. 2) -- (4-3) * 16 = 16 space
          s8  = (  s4 .&. 0x0F0F0F0F0F0F0F0F) + -- 0x0707..  
                (( s4 .&. 0xF0F0F0F0F0F0F0F0) .>>. 4) -- 0x7070..  -- (8-4) * 8 = 24 space
          test1 = (n-s8) * 0x0101010101010101 + s8
          test2 = (test1 .&. 0x8080808080808080)
          test3 = test2 .>>. (64 - ((test2 .>>. 56) .&. 0xFF) * 8)
{-# INLINE selectInt' #-}

-- |
-- >>> selectByte 0x2 True 1
selectByte :: Int -> Bool -> Int -> Int
selectByte b True n = d
    where d = b * 0x0101010101010101 .&. 0x8040201008040201
          d' = (d .<<. 7)

-- |
-- >>> expandCumSum8' . selectInt' 0x10000000F0471201 True <$> [0..14]

expandCumSum8' :: Int -> [Int]
expandCumSum8' x = P.takeWhile (128>) [x .>>. (i*8) .&. 0xFF | i<- [0..7]]

expandCumSum8 :: Int -> [Int]
expandCumSum8 x = [(x .>>. (i*8)) .&. 0xFF | i<- [0..7]]

-- 64bit Only
popCumSum8 :: Int -> Int
popCumSum8 s1 = let s2 = (s1 .&. 0x5555555555555555) +
                         ((s1 .&. 0xAAAAAAAAAAAAAAAA) .>>. 1)
                    s4 = (s2 .&. 0x3333333333333333) + 
                         ((s2 .&. 0xCCCCCCCCCCCCCCCC) .>>. 2)
                    s8 = (s4 .&. 0x0F0F0F0F0F0F0F0F) +
                         ((s4 .&. 0xF0F0F0F0F0F0F0F0) .>>. 4)
                    cs8 = s8 * 0x0101010101010101
                 in cs8 

top8 :: Int -> Int
top8 x = x .>>. (bitW - 8)
{-# INLINE top8 #-}

-- |
-- >>> reify $ showBit  0x03F566ED27179461
showBit = showIntAtBase 2 conv
    where conv 0 = '0'
          conv 1 = '1'


-- |
-- >>> codes . 
codes :: [Word64]
codes = [(2 ^ 23) `mod` 0xC75]


reify :: Monoid a => (a -> a) -> a
reify = ($ mempty)

-- >>> popCount' 0xFF7F3F1F0F070301
-- 36
popCount' :: Int -> Int
popCount' = top8 . popCumSum8

popCumSumFold :: [Int] -> (Int,[Int])
popCumSumFold xs = (a,bs)
    where (as,bs) = P.splitAt 8 xs
          shifts = P.zipWith (\i p -> p .<<. i )  
                     [0,9 .. 9*6] (P.map popCount' xs)
          a = P.sum shifts * 0x0040201008040201

-- |
-- >>> let a = fst $ popCumSumFold [0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80]

unfold9bit :: Int -> [Int]
unfold9bit x = L.unfoldr un x
    where un 0 = Nothing
          un x = Just (x .&. 0x1FF,x .>>. 9)

main = return ()
