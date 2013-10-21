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

type N = Int

class RSV v where
    type E v
    size :: v -> N
    access :: v -> N -> E v
    rank :: v -> N -> E v -> N
    select :: v -> N -> E v -> N

instance Eq a => RSV [a] where
    type E [a] = a
    size = P.length
    access = (!!)
    rank xs n e = size . P.filter (== e) $ P.take n xs
    select xs 0 e = 0
    select xs n e =  fst (P.filter (\p -> snd p == e) (P.zip [0..] xs) !! (n-1))


-- |
-- >>> select (V.fromList [1,3,7,0xF] :: Vector Int) 8 True

-- |
-- >>> rank (V.fromList [1,3,7,0xF] :: Vector Int) 194 True
--

instance RSV (Vector Int) where
    type E (Vector Int) = Bool
    size = V.length
    access v i = testBit ofs (v ! ind) 
        where (ind,ofs) = i `divMod` (bitSize i) 
    rank _ 0 _ = 0
    rank v i True = ofscnt + V.sum (V.map popCount (V.unsafeSlice 0 ind v))
        where ofscnt = popCount ((v ! ind) `unsafeShiftL` (bitSize i - ofs -1))
              (ind,ofs) = (i-1) `divMod` bitSize i
    rank v i False = i - (rank v i True)
    select v i True = ind + ofs
        where v1 = V.takeWhile (< i) vp
              vp = prescanl' (+) 0 (V.map popCount v)
              ind = (bitSize i) * (V.length v1 - 1)
              ofs = select64 (v!(V.length v1 - 1)) (i - V.last v1 ) True

-- |
-- >>> select64 8 1 True

select64 :: Int -> Int -> Bool -> Int
select64 x 0 b = 0
select64 x n b = 1 + fst (P.filter (f . snd)  [(s,testBit (unsafeShiftR x s) 0) | s <- [0..63]] !! (n-1))
      where f = if b then id else not

main = pure ()

