{-# LANGUAGE FlexibleContexts,FlexibleInstances,MultiParamTypeClasses,TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Ix
import Data.Bits
import Data.Word
import Data.Array.Unboxed
import Control.Applicative
import Data.List
import Data.Monoid
import Data.Bifunctor
import Data.Biapplicative
import Data.Bifoldable
import Data.Bool
import Data.Maybe
import Data.Functor.Foldable
import Control.Monad.Fix

type N = Int

class Dict d where
    type Code d
    size :: d -> N
    access :: d -> N -> Code d
    rank :: d -> Code d -> N -> N
    select :: d -> Code d -> N -> N

class Dict d => Wavelet d where
    type W d
    wavelet :: d -> W d


instance Eq a => Dict [a] where
    type Code [a] = a
    size = length
    access = (!!)
    rank xs e n = length (filter (== e) (take n xs))
    select xs e n = fst ((filter (\(i,x)-> x == e) (zip [0..] xs)) !! n)

-- |
-- >>> wavelet ([15,3,7,1] :: [Int])

instance Bits b => Wavelet [b] where
    type W [b] = [b]
    wavelet xs = foldr1 (zipWith (.|.)) (f 0 xs)
        where f n xs | n == wsize = [] 
                     | otherwise = ((bit n .&.) <$> xs) : f (n+1) (bifold (classify (flip testBit n) xs))
              wsize = bitSize (undefined :: b)


classify :: (a -> Bool) -> [a] -> ([a],[a])
classify _ [] = ([],[])
classify f (x:xs) | f x = second (x:) $ classify f xs
                  | otherwise = first (x:) $ classify f xs

main = pure ()
