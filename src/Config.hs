module Config where

import Foreign.C.Types (CInt)
import Data.Word (Word32)

-- TAMAÑOS
heroSize :: CInt
heroSize = 32

tileSizeSource :: CInt
tileSizeSource = 16

tilesetCols :: CInt
tilesetCols = 6

heroStartY :: CInt
heroStartY = 144

-- PANTALLA
screenSize :: CInt
screenSize = 64

walkSpeed :: CInt
walkSpeed = 4

aggroRange :: CInt
aggroRange = 320

-- NUEVO: Rango de visión del Zombie (480px = 7.5 bloques)
zombieAggroRange :: CInt
zombieAggroRange = 480

attackRange :: CInt
attackRange = 60

windowW, windowH :: CInt
(windowW, windowH) = (1280, 720)

-- TIEMPOS Y BALANCE
respawnTime :: Word32
respawnTime = 5000

regenTime :: Word32
regenTime = 2000

xpToLevelUp :: Int
xpToLevelUp = 100

enemyAttackInterval :: Word32
enemyAttackInterval = 1000

-- ANIMACIÓN
heroFramesPerDirection :: CInt
heroFramesPerDirection = 4

heroRowDown :: CInt
heroRowDown = 0 * heroSize

heroRowLeft :: CInt
heroRowLeft = 1 * heroSize

heroRowRight :: CInt
heroRowRight = 3 * heroSize

heroRowUp :: CInt
heroRowUp = 2 * heroSize
