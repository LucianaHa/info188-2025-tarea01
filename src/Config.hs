module Config where

import Foreign.C.Types (CInt)
import Data.Word (Word32) -- ¡Faltaba esta línea!

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
-- Aumentamos el ancho para dejar espacio al Log en el futuro
screenSize :: CInt
screenSize = 64

walkSpeed :: CInt
walkSpeed = 4

windowW, windowH :: CInt
(windowW, windowH) = (1280, 720)

-- TIEMPOS Y BALANCE
respawnTime :: Word32
respawnTime = 5000 -- 5 segundos para revivir

regenTime :: Word32
regenTime = 2000 -- Regenerar vida cada 2 segundos

xpToLevelUp :: Int
xpToLevelUp = 100 -- XP necesaria para nivel 2