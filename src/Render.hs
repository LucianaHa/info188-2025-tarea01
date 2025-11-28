module Render where

import qualified SDL
import Linear (V2(..), V4(..), (^-^), (^+^))
import Linear.Affine (Point(..))
import Control.Monad.State
import Control.Monad (when, forM_)
import qualified Data.Map as M
import Foreign.C.Types (CInt)
import Data.Word (Word8) 

import Types
import Config
import Maps

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

renderEntity :: SDL.Renderer -> SDL.Texture -> Entity -> V2 CInt -> Game ()
renderEntity r tex ent cameraOffset = do
    let charIndex = characterIndexFromClass (entClass ent)
    let srcX = charIndex * heroSize
    let srcY = heroStartY 
    
    let srcRect = SDL.Rectangle (P (V2 srcX srcY)) (V2 heroSize heroSize)
    let screenPos = entPos ent ^-^ cameraOffset
    let destRect = SDL.Rectangle (P screenPos) (V2 screenSize screenSize)
    
    -- DIBUJAR SPRITE (Limpio, sin cuadros verdes)
    SDL.copy r tex (Just srcRect) (Just destRect)
    
    -- BARRA DE VIDA (Solo si no está full o es enemigo enojado)
    when (entHp ent < entMaxHp ent || entAggro ent) $ do
        drawHealthBar r screenPos (entHp ent) (entMaxHp ent)

-- NUEVO: Dibujar Pantalla de Título (SIMPLIFICADA)
drawTitleScreen :: SDL.Renderer -> AssetManager -> Int -> Game ()
drawTitleScreen r texs sel = do
    -- 1. Dibujar Fondo (Imagen cargada, que ya contiene los botones)
    case M.lookup "background" texs of
        Just bgTex -> SDL.copy r bgTex Nothing Nothing -- Dibuja en toda la pantalla
        Nothing -> do -- Si falla la carga, fondo azul
            SDL.rendererDrawColor r SDL.$= V4 0 0 50 255 
            SDL.clear r

    -- Nota: Ya no se dibujan botones de color, solo se usa el teclado para navegar.

    SDL.present r

-- Función drawButton eliminada, ya no es necesaria.

render :: Game ()
render = do
    mode <- gets gameMode
    st <- get
    let r = renderer st

    case mode of
        TitleScreen -> 
            drawTitleScreen r (assets st) (menuSelection st)
        
        Playing -> do
            let texs = assets st
            let center = V2 (windowW `div` 2) (windowH `div` 2)
            let pCenter = V2 (screenSize `div` 2) (screenSize `div` 2)
            let cameraOffset = entPos (player st) ^-^ center ^+^ pCenter

            -- FONDO OSCURO DE MAZMORRA 
            SDL.rendererDrawColor r SDL.$= V4 15 15 20 255 
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
                case getTileRect tileID of
                    Nothing -> return ()
                    Just srcRect -> do
                        let worldPos = V2 (fromIntegral x * screenSize) (fromIntegral y * screenSize)
                        let screenPos = worldPos ^-^ cameraOffset
                        let (V2 sx sy) = screenPos
                        
                        -- DIBUJAR MAPA 
                        when (sx > -screenSize && sx < windowW && sy > -screenSize && sy < windowH) $ do
                            let destRect = SDL.Rectangle (P screenPos) (V2 screenSize screenSize)
                            SDL.copy r tex (Just srcRect) (Just destRect)