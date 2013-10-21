{-# LANGUAGE FlexibleInstances #-}
module BV where

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

type W64 = Word64
type W32 = Word32
type W16 = Word16
type W8 = Word8
newtype BV = BV { uarray :: UArray W64 W64 }

showBits :: W64 -> String
showBits w = [if (w `unsafeShiftR` i) .&. 1 == 0 then '0' else '1' | i <- [0..63]]

(.>>.) :: Bits a => a -> Int -> a
(.>>.) = unsafeShiftR
(.<<.) :: Bits a => a -> Int -> a
(.<<.) = unsafeShiftL
(.^.) :: Bits a => a -> a -> a
(.^.) = xor

(.<=.) :: Bits a => a -> a -> a
x .<=. y = (((y .|.  

mask10101010 :: W64
mask10101010 = 0xAAAAAAAAAAAAAAAA
mask00110011 :: W64
mask00110011 = 0x3333333333333333
mask11110000 :: W64
mask11110000 = 0xF0F0F0F0F0F0F0F0
mask00000001 :: W64
mask00000001 = 0x0101010101010101

select9 :: W64 -> W64 -> W64
select9 r x = let s0 = x - ((x .&. mask10101010) .>>. 1)
                  s1 = (s0 .&. mask00110011) + ((x .>>. 2) .&. mask00110011)
                  s2 = ((s1 + (s1 .>>. 4)) .&. mask11110000) * mask00000001
                  b = ((s2 

-- |
-- >>> showBits 234

-- |
-- >>> showBits (popc 234)
