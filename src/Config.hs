module Config where

import Foreign.C.Types (CInt)
import Data.Word (Word32)

-- TAMAÑOS
heroSize :: CInt
heroSize = 16

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

-- Rango de visión del enemigo (320px = 5 bloques)
aggroRange :: CInt
aggroRange = 320

-- Rango de ataque (cuerpo a cuerpo, un poco menos de un bloque para que se acerque bien)
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

-- NUEVO: Tiempo entre ataques del enemigo (1 segundo)
enemyAttackInterval :: Word32
enemyAttackInterval = 1000
