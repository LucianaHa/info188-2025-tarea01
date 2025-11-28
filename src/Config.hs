module Config where

import Foreign.C.Types (CInt)

-- TAMAÑOS
heroSize :: CInt
heroSize = 16 

-- ...
tileSizeSource :: CInt
tileSizeSource = 16 

-- ¡PRUEBA CAMBIAR ESTE NÚMERO!
tilesetCols :: CInt
tilesetCols = 6 -- O prueba con 10 si tu imagen es mas ancha
-- ...
-- Coordenada Y donde empiezan los personajes (Tu dato)
heroStartY :: CInt
heroStartY = 144

screenSize :: CInt
screenSize = 64

walkSpeed :: CInt
walkSpeed = 4

windowW, windowH :: CInt
(windowW, windowH) = (1280, 720)