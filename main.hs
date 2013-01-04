{-# LANGUAGE ImplicitParams #-}
import Graphics.FreeGame
import Data.Vect

jumpReady :: (?player :: Picture) => Float -> Game ()
jumpReady x = do
    let pos = Vec2 x 468
    drawPicture $ Translate pos ?player
    mouse <- getMouseState
    tick
    if leftButton mouse
        then jump pos ((mousePosition mouse &- pos) &* 0.1)
        else jumpReady x

jump :: (?player :: Picture) => Vec2 -> Vec2 -> Game ()
jump pos@(Vec2 x y) vel@(Vec2 dx dy) = do
    drawPicture $ Translate pos ?player
    let dx' | x <= 0 = abs dx
            | x >= 640 = -(abs dx)
            | otherwise = dx
        dy' | y <= 0 = abs dy
            | otherwise = dy + 1
        vel' = Vec2 dx' dy'
    tick
    if (y + dy) >= 468
       then jumpReady x
       else jump (pos &+ vel) vel'

initGame :: (?player :: Picture) => Game ()
initGame = do
    x <- randomness (0, 640)
    jumpReady x

main :: IO (Maybe ())
main = runGame defaultGameParam $ do
    pic <- loadPictureFromFile "images/player.png"
    let ?player = pic
    run initGame
  where
    run game = do
        game' <- untickGame game
        tick
        run game'
