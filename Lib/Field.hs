{-# LANGUAGE ImplicitParams #-}

module Lib.Field where

import Control.Monad (forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Vect
import Graphics.FreeGame

import Lib.Util

type Field = Map (Int, Int) Block

data Block = B | N | S | G deriving (Eq)

field :: Field
field = fromLists exampleField

exampleField :: [[Block]]
exampleField =
    [ [B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B]
    , [B, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, B]
    , [B, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, B]
    , [B, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, B]
    , [B, N, N, N, B, B, B, B, B, N, N, N, N, N, N, N, N, N, N, B]
    , [B, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, B]
    , [B, N, N, N, N, N, N, N, N, N, N, N, B, N, N, N, N, N, N, B]
    , [B, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, B]
    , [B, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, B]
    , [B, N, S, N, N, N, N, N, N, N, B, B, B, B, B, N, N, N, N, B]
    , [B, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, B]
    , [B, N, N, N, N, B, B, N, N, N, N, N, N, N, N, N, N, N, N, B]
    , [B, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, B]
    , [B, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, G, B]
    , [B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B]
    ]

fromLists :: [[Block]] -> Field
fromLists = Map.fromList . concatMap f . zip [1..] . map (zip [1..])
  where
    f (_, []) = []
    f (y, (x, b):xbs) = ((x, y), b) : f (y, xbs)

startPos :: Vec2
startPos = toVec2 pos
  where
    pos = head $ blockPos field S

blockPos :: Field -> Block -> [(Int, Int)]
blockPos field' block = catMaybes $ map f keys
  where
    keys = Map.keys field'
    f k = case Map.lookup k field' of
        Just b -> if b == block then Just k else Nothing
        Nothing -> Nothing

drawField :: (?block :: Picture) => Game ()
drawField = forM_ keys $ \k -> drawBlock k $ f k
  where
    keys = Map.keys field
    f :: (Int, Int) -> Block
    f = flip (Map.findWithDefault N) field

drawBlock :: (?block :: Picture) => (Int, Int) -> Block -> Game ()
drawBlock pos B = drawPicture $ Translate (toVec2 pos) ?block
drawBlock _ _ = return ()
