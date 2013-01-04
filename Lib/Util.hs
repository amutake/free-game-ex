module Lib.Util where

import Data.Vect

import Lib.Settings

toVec2 :: (Int, Int) -> Vec2
toVec2 = uncurry Vec2 . mapT f
  where
    mapT g (a, b) = (g a, g b)
    f x = blockSize * (fromIntegral x) - blockSize / 2

toTuple :: Vec2 -> (Int, Int)
toTuple (Vec2 x y) = (f x, f y)
  where
    f a = round $ (a + blockSize / 2) / blockSize

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd = fmap

mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup f = mapSnd f . mapFst f
