module Render where

import qualified SDL
import Linear (V2(..), V4(..), (^-^), (^+^))
import Linear.Affine (Point(..))
import Control.Monad.State
import Control.Monad (when, forM_)
import qualified Data.Map as M
import Foreign.C.Types (CInt)

import Types
import Config
import Maps

-- Índice de Clases
characterIndexFromClass :: Clase -> CInt
characterIndexFromClass Guerrero = 0
characterIndexFromClass Mago     = 1
characterIndexFromClass Asesino  = 5
characterIndexFromClass Orco     = 3 
characterIndexFromClass Esqueleto = 4

getTileRect :: Int -> Maybe (SDL.Rectangle CInt)
getTileRect id 
    | id < 0 = Nothing
    | otherwise = Just rect
  where
    col = fromIntegral (id `mod` fromIntegral tilesetCols)
    row = fromIntegral (id `div` fromIntegral tilesetCols)
    x = col * tileSizeSource
    y = row * tileSizeSource
    rect = SDL.Rectangle (P (V2 x y)) (V2 tileSizeSource tileSizeSource)

-- DIBUJAR BARRA DE VIDA
drawHealthBar :: SDL.Renderer -> V2 CInt -> Int -> Int -> Game ()
drawHealthBar r (V2 x y) hp maxHp = do
    let width = screenSize
    let height = 6
    let yOffset = -10

    let barBg = SDL.Rectangle (P (V2 x (y + yOffset))) (V2 width height)
    let pct = fromIntegral hp / fromIntegral maxHp :: Float
    let greenW = floor (fromIntegral width * pct) :: CInt
    let barFg = SDL.Rectangle (P (V2 x (y + yOffset))) (V2 greenW height)

    SDL.rendererDrawColor r SDL.$= V4 255 0 0 255
    SDL.fillRect r (Just barBg)
    SDL.rendererDrawColor r SDL.$= V4 0 255 0 255
    SDL.fillRect r (Just barFg)

-- RENDERIZAR ENTIDAD
renderEntity :: SDL.Renderer -> SDL.Texture -> Entity -> V2 CInt -> Game ()
renderEntity r tex ent cameraOffset = do
    let charIndex = characterIndexFromClass (entClass ent)
    let srcX = charIndex * heroSize
    let srcY = heroStartY 
    
    let srcRect = SDL.Rectangle (P (V2 srcX srcY)) (V2 heroSize heroSize)
    let screenPos = entPos ent ^-^ cameraOffset
    let destRect = SDL.Rectangle (P screenPos) (V2 screenSize screenSize)
    
    -- 1. DIBUJAR SPRITE
    SDL.copy r tex (Just srcRect) (Just destRect)
    
    -- 2. DIBUJAR CUADRO DE DEBUG (Borde Verde)
    -- Si ves este cuadro pero no el personaje, la textura o heroStartY están mal.
    SDL.rendererDrawColor r SDL.$= V4 0 255 0 255 -- Verde brillante
    SDL.drawRect r (Just destRect) 
    
    -- 3. BARRA DE VIDA
    when (entHp ent < entMaxHp ent || entAggro ent) $ do
        drawHealthBar r screenPos (entHp ent) (entMaxHp ent)

render :: Game ()
render = do
    st <- get
    let r = renderer st
    let texs = assets st
    
    let center = V2 (windowW `div` 2) (windowH `div` 2)
    let pCenter = V2 (screenSize `div` 2) (screenSize `div` 2)
    let cameraOffset = entPos (player st) ^-^ center ^+^ pCenter

    -- FONDO AZUL (Para saber si la ventana funciona)
    -- Si ves negro, SDL no está renderizando. Si ves azul, vamos bien.
    SDL.rendererDrawColor r SDL.$= V4 0 0 100 255 
    SDL.clear r

    case M.lookup "dungeon" texs of
        Just texDungeon -> do
            renderLayer r texDungeon mapaSuelo cameraOffset
            
            forM_ (enemies st) $ \e -> renderEntity r texDungeon e cameraOffset
            renderEntity r texDungeon (player st) cameraOffset
            
        Nothing -> return ()

    SDL.present r

  where
    renderLayer r tex matriz cameraOffset = do
        forM_ (zip [0..] matriz) $ \(y, row) -> do
            forM_ (zip [0..] row) $ \(x, tileID) -> do
                -- DEBUG MAPA: Si el tileID es válido, intenta dibujar
                case getTileRect tileID of
                    Nothing -> return ()
                    Just srcRect -> do
                        let worldPos = V2 (fromIntegral x * screenSize) (fromIntegral y * screenSize)
                        let screenPos = worldPos ^-^ cameraOffset
                        let (V2 sx sy) = screenPos
                        
                        when (sx > -screenSize && sx < windowW && sy > -screenSize && sy < windowH) $ do
                            let destRect = SDL.Rectangle (P screenPos) (V2 screenSize screenSize)
                            
                            -- Dibujar Textura
                            SDL.copy r tex (Just srcRect) (Just destRect)
                            
                            -- DEBUG: Si es pared (ID 7), dibuja un borde gris
                            when (tileID == 7) $ do
                                SDL.rendererDrawColor r SDL.$= V4 100 100 100 255
                                SDL.drawRect r (Just destRect)