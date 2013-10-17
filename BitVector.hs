{-# LANGUAGE FlexibleInstances #-}
module BitVector where

import Data.Array.Unboxed
import Data.Array.ST
import Data.String
import Data.List
import Data.Maybe
import Data.Word
import Data.Bits
import Control.Monad.ST
import Data.Binary
import qualified Data.Foldable as F

-- newtype BV = BV {getBV :: STUArray Word64 Word64 }


type STBV s = ST s (STUArray s Word64 Word64)

newtype BitVector = BitVector { uarray :: UArray Word64 Word64 }

newtype SDBitVector = SDBitVector {table :: (UArray Word64 Word64,UArray Word16 Word64, UArray Word64 Word16)} 

class BV v where
    size :: v -> Word64
    access :: Word64 -> v -> Bool

class BV v => RSBV v where
    rank :: Bool -> Word64 -> v -> Word64
    select :: Bool -> Word64 -> v -> Word64

popc :: Word64 -> Word64
popc = fromIntegral . popCount

instance BV ([Bool]) where
    size = fromIntegral . length
    access i = (!! fromIntegral i)

instance RSBV ([Bool]) where
    rank b i = fromIntegral . length . filter (f) . take (fromIntegral i)
        where f = if b then id else not
    select b i xs = fromIntegral . snd . (!! fromIntegral i) . filter (f . fst) $ zip xs [0..]
        where f = if b then id else not


instance BV BitVector where
    size = (`unsafeShiftL` 6) . fromIntegral . rangeSize . bounds . uarray
    access pos (BitVector ua) = (((ua ! index) `unsafeShiftR` offset) .&. 1) /= 0
        where index = fromIntegral (pos `unsafeShiftR` 6)
              offset = fromIntegral (pos .&. 0x3F)

instance BV SDBitVector where
    size = size . weak
    access pos = access pos . weak

-- |
-- >>> let a = mkBitVector (take 5000 (cycle [True,False,False,True,False]))
-- >>> rank True 0 a
-- 0
-- >>> let a' = mkSDBitVector a
-- >>> [(rank True i a',i) | i <- [61..82]]
-- [(25,61),(25,62),(25,63),(26,64),(26,65),(27,66),(27,67),(27,68),(28,69),(28,70),(29,71),(29,72),(29,73),(30,74),(30,75),(31,76),(31,77),(31,78),(32,79),(32,80),(33,81),(33,82)]
-- >>> [select True i a' | i <- [25 .. 33]]
-- [61,64,66,69,71,74,76,79,81]

instance RSBV BitVector where
    rank True i (BitVector arr) = cnta - cntb
        where w = i `unsafeShiftR` 6
              ofs = fromIntegral (i .&. 0x3F)
              cnta = foldl' (\a x -> a + popc x) 0 (elems (ixmap (0,w) id arr))
              cntb = popc $ ((arr ! w) `unsafeShiftR` (ofs))
    rank False i bv = i - rank True i bv
    select _ _ _ = error "yet"

instance RSBV SDBitVector where
    rank True i (SDBitVector (bv,ld,sd)) = cntld + cntsd - cntofs
        where ind = i `unsafeShiftR` 6
              lind = ind `unsafeShiftR` 4
              ofs = fromIntegral $ i .&. 0x3F
              cntld = if lind == 0 then 0 else ld ! fromIntegral (lind-1)
              cntsd = fromIntegral $ sd ! ind
              cntofs = popc ((bv ! ind) `unsafeShiftR` ofs) 
    rank False i bv = i - rank True i bv
    select True i (SDBitVector (bv,ld,sd)) = ans
      where (lds,lde) = bounds ld
            binsearch s e | s+1 == e = if i < (ld ! s) then s else s+1
                          | i < (ld ! m) = binsearch s m
                          | otherwise = binsearch m e 
                          where m = (s+e+1) `unsafeShiftR` 1
            indl = binsearch lds lde
            base = if indl == 0 then 0 else ld ! (indl-1)
            diff = fromIntegral (i - base)
            indl' = fromIntegral indl `unsafeShiftL` 4
            binsearch' s e | s+1 == e = if diff < (sd ! s) then s else s+1
                           | diff < (sd ! m) = binsearch' s m
                           | otherwise     = binsearch' m e 
                           where m = (s+e+1) `unsafeShiftR` 1
            inds' = binsearch' indl' (indl' + 15)
            base' = if inds' == 0 then 0 else sd ! (inds'-1)
            diff' = fromIntegral (diff - base')
            ofs = fromIntegral (calcofs diff' (bv ! inds'))
            ans = (inds' `unsafeShiftL` 6) + ofs

showBits :: Word64 -> String
showBits w = [if (w `unsafeShiftR` i) .&. 1 == 0 then '0' else '1' | i <- [0..63]]

calcofs :: Word8 -> Word64 -> Word64
calcofs 1 0 = 0
calcofs 1 word = fromIntegral . head $ [ i+1 | i <- [0..63], (word .&. (-word)) `unsafeShiftR` i == 1 ]
calcofs ofs word = calcofs (ofs-1) $ word .&. complement (word .&. (-word))

mkBitVector :: [Bool] -> BitVector
mkBitVector bs = BitVector (listArray (0, fromIntegral (length ws) - 1) ws)
    where f [] = []
          f bs' = let (as,bs'') = splitAt 64 bs' in
                    g as : f bs''
          g as = foldl' (+) 0 (zipWith (\b ofs -> toW b `unsafeShiftL` ofs ) as [0 .. ])
          toW True = 1
          toW False = 0
          ws = f bs :: [Word64]

showBitVector :: BitVector -> String
showBitVector bv = map (showBool . flip access bv) [0..size bv-1]
    where showBool True = '1'
          showBool False = '0'

mkSDBitVector :: BitVector -> SDBitVector
mkSDBitVector (BitVector bv) = SDBitVector (bv,ld,sd)
    where (s,e) = bounds $ bv
          sdlist = concatMap f [s,s+16..e]
            where f i = scanl1 (+) [ fromIntegral (popc (bv ! (i+j))) | j <- [0..15], i+j <= e ]
          sd = listArray (s,e) sdlist :: UArray Word64 Word16
          ldlist = scanl1 (+) [ fromIntegral (sd ! j) | j <- [15,31..e] ]
          ld = listArray (0,fromIntegral (length ldlist - 1)) ldlist :: UArray Word16 Word64

weak :: SDBitVector -> BitVector
weak (SDBitVector (bv,_,_)) = BitVector bv

instance Show SDBitVector where
    show = show . weak

instance Show BitVector where
    show = showBitVector 

-- |
-- >>> access 1 (mkBitVector [True,False,False,True,False])
-- False
-- >>> rank True 4 [True,False,False,True,False]
-- 2
-- >>> select True 1 [True,False,False,True,False]
-- 3
-- >>> rank False (select False 1 [True,False,False,True,False]) [True,False,False,True,False]
-- 1


zeros :: Word64 -> STBV s
zeros l = do
    t <- newArray (0, l `unsafeShiftR` 64 ) 0 :: STBV s
    return t

test2 :: (Num i, Num e, Ix i, MArray a e m) => a i e -> m (a i e)
test2 a = do
    writeArray a 0 100
    return a
