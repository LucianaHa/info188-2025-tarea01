module Config where

import Foreign.C.Types (CInt)
import Data.Word (Word32) 

-- TAMAÑOS
heroSize :: CInt
heroSize = 32 -- 128 / 4 = 32 Píxeles por sprite (Héroe y Ogro)

tileSizeSource :: CInt
tileSizeSource = 16 -- Tamaño original de los tiles del dungeon.png

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

-- NUEVAS CONSTANTES PARA ANIMACIÓN DEL HÉROE
heroFramesPerDirection :: CInt
heroFramesPerDirection = 4 

-- FILAS DE ANIMACIÓN EN hero.png / ogre.png
heroRowDown :: CInt
heroRowDown = 0 * heroSize 

heroRowLeft :: CInt
heroRowLeft = 1 * heroSize 

heroRowRight :: CInt
heroRowRight = 3 * heroSize -- Ajustado

heroRowUp :: CInt
heroRowUp = 2 * heroSize -- Ajustado