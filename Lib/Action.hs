{-# LANGUAGE ImplicitParams #-}
module Lib.Action where

import qualified Data.Map as Map
import Data.Vect
import Graphics.FreeGame

import Lib.Field
import Lib.Util

data Current = Ground | Air deriving (Show, Eq)

action :: (?player :: Picture, ?block :: Picture)
       => Vec2 -> Vec2 -> Current -> Game ()
action pos vel@(Vec2 dx _) Ground = do

    drawPicture $ Translate pos ?player
    drawField

    keyU <- askInput KeyUp
    keyR <- askInput KeyRight
    keyL <- askInput KeyLeft

    let dx' | keyR && leftCollision pos = dx + 0.1
            | keyL && rightCollision pos = dx - 0.1
            | rightCollision pos = 0
            | leftCollision pos = 0
            | keyR = dx + 0.1
            | keyL = dx - 0.1
            | otherwise = slow dx
        dy' | keyU = -8
            | not $ downCollision pos = 0.1
            | otherwise = 0
        vel' = Vec2 dx' dy'
        next = if keyU || not (downCollision pos) then Air else Ground
    tick
    action (pos &+ vel) vel' next
action pos vel@(Vec2 dx dy) Air = do
    drawPicture $ Translate pos ?player
    drawField

    keyR <- askInput KeyRight
    keyL <- askInput KeyLeft

    let dx' | keyR && leftCollision pos = dx + 0.1
            | keyL && rightCollision pos = dx - 0.1
            | rightCollision pos = 0
            | leftCollision pos = 0
            | keyR = dx + 0.1
            | keyL = dx - 0.1
            | otherwise = slow dx
        dy' | downCollision pos = 0
            | dy < 0 && upCollision pos = -dy
            | otherwise = dy + 0.3
        vel' = Vec2 dx' dy'
        next = if downCollision pos then Ground else Air
    tick
    action (pos &+ vel) vel' next

slow :: Float -> Float
slow x
  | x > 0 = x - 0.1
  | x < 0 = x + 0.1
  | otherwise = x

collision :: Vec2 -> Bool
collision pos = upCollision pos ||
                downCollision pos ||
                rightCollision pos ||
                leftCollision pos

upCollision :: Vec2 -> Bool
upCollision pos = Map.findWithDefault N upPos field == B
  where
    upPos = mapSnd (\x -> x - 1) $ toTuple pos

downCollision :: Vec2 -> Bool
downCollision pos = Map.findWithDefault N downPos field == B
  where
    downPos = mapSnd (+ 1) $ toTuple pos

rightCollision :: Vec2 -> Bool
rightCollision pos = Map.findWithDefault N rightPos field == B
  where
    rightPos = mapFst (+ 1) $ toTuple pos

leftCollision :: Vec2 -> Bool
leftCollision pos = Map.findWithDefault N leftPos field == B
  where
    leftPos = mapFst (\x -> x - 1) $ toTuple pos
