module Lib.Settings where

import Graphics.FreeGame

gameParam :: GameParam
gameParam = defaultGameParam
    { windowTitle = "free-game-ex"
    }

blockSize :: Float
blockSize = 32
