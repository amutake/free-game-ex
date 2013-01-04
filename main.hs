{-# LANGUAGE ImplicitParams #-}
import Graphics.FreeGame
import Data.Vect

import Lib.Action
import Lib.Field
import Lib.Settings

test :: (?player :: Picture, ?block :: Picture) => Game ()
test = do
    drawPicture $ Translate startPos ?player
    drawField
    tick
    test

main :: IO (Maybe ())
main = runGame gameParam $ do
    player <- loadPictureFromFile "images/player.png"
    block <- loadPictureFromFile "images/block.png"
    let ?player = player
        ?block = block
    run $ action startPos (Vec2 0 0) Air
  where
    run game = do
        game' <- untickGame game
        tick
        run game'
