module Render where

import qualified SDL
import qualified SDL.Font
import Linear (V2(..), V4(..), (^-^), (^+^))
import Linear.Affine (Point(..))
import Control.Monad.State
import Control.Monad (when, forM_, unless, forM)
import qualified Data.Map as M
import qualified Data.Text as T
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
characterIndexFromClass Zombie    = 4
characterIndexFromClass Vaca      = 4

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

itemTilesetCols :: CInt
itemTilesetCols = 8

-- NUEVA FUNCIÓN DE RECORTE: Usa 8 columnas en lugar de tilesetCols (6)
getItemTileRect :: Int -> Maybe (SDL.Rectangle CInt)
getItemTileRect id
    | id < 0 = Nothing
    | otherwise = Just rect
  where
    col = fromIntegral (id `mod` fromIntegral itemTilesetCols)
    row = fromIntegral (id `div` fromIntegral itemTilesetCols)
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

renderLog :: SDL.Renderer -> Maybe SDL.Font.Font -> [String] -> Game ()
renderLog _ Nothing _ = return ()
renderLog r (Just font) logs = do
    let colorTexto = V4 255 255 255 255
    let colorFondo = V4 0 0 0 180

    let panelW = 500
    let panelH = 120
    let margin = 20

    let baseX = fromIntegral windowW - panelW - margin
    let baseY = fromIntegral windowH - panelH - margin

    SDL.rendererDrawBlendMode r SDL.$= SDL.BlendAlphaBlend
    SDL.rendererDrawColor r SDL.$= colorFondo

    let bgRect = SDL.Rectangle (P (V2 baseX baseY)) (V2 panelW panelH)
    SDL.fillRect r (Just bgRect)

    SDL.rendererDrawBlendMode r SDL.$= SDL.BlendNone

    let recentLogs = take 6 logs
    let lineHeight = 18
    let textMarginX = 10
    let textMarginY = 10

    forM_ (zip [0..] recentLogs) $ \(i, msg) -> do
        let text = T.pack msg
        surface <- SDL.Font.blended font colorTexto text
        dims <- SDL.surfaceDimensions surface

        let (V2 w h) = dims
        let textW = fromIntegral w
        let textH = fromIntegral h

        let posX = baseX + textMarginX
        let posY = baseY + textMarginY + (fromIntegral i * lineHeight)

        texture <- SDL.createTextureFromSurface r surface

        let destRect = SDL.Rectangle (P (V2 posX posY)) (V2 textW textH)

        SDL.copy r texture Nothing (Just destRect)
        SDL.destroyTexture texture
        SDL.freeSurface surface

queryTextureSafe :: SDL.Texture -> Game (CInt, CInt)
queryTextureSafe tex = do
    info <- SDL.queryTexture tex
    return (SDL.textureWidth info, SDL.textureHeight info)

renderEntity :: SDL.Renderer -> M.Map String SDL.Texture -> Entity -> V2 CInt -> Game ()
renderEntity r texs ent cameraOffset = do
    let entClass' = entClass ent
    let screenPos = entPos ent ^-^ cameraOffset

    let destRect = SDL.Rectangle (P screenPos) (V2 screenSize screenSize)

    let (texKey, frameIndex) = case entClass' of
                                    Guerrero -> ("hero", entAnimFrame ent)
                                    Orco     -> ("ogre", entAnimFrame ent)
                                    Zombie   -> ("zombie", entAnimFrame ent)
                                    Vaca     -> ("cow", entAnimFrame ent)
                                    _        -> ("dungeon", fromIntegral (characterIndexFromClass entClass'))

    let spriteSize = if texKey == "dungeon" then tileSizeSource else heroSize

    case M.lookup texKey texs of
        Nothing -> return ()

        Just tex -> do
            -- 1. CALCULAR RECORTE DEL SPRITE (Lógica de Animación Avanzada)
            srcRect <- if texKey == "dungeon"
                then
                    let
                        srcX = fromIntegral frameIndex * spriteSize
                        srcY = heroStartY
                    in return $ SDL.Rectangle (P (V2 srcX srcY)) (V2 spriteSize spriteSize)
                else do
                    -- La lógica avanzada de animación (filas/columnas)
                    let cols = 4
                    let rows = 4
                    (texW, texH) <- queryTextureSafe tex

                    let cellW = fromIntegral texW `div` cols
                    let cellH = fromIntegral texH `div` rows

                    let row = case entDir ent of
                                Abajo     -> 0
                                Izquierda -> 1
                                Arriba    -> 2
                                Derecha   -> 3

                    let col = fromIntegral (entAnimFrame ent)

                    let srcX = col * cellW
                    let srcY = row * cellH

                    return $ SDL.Rectangle (P (V2 srcX srcY)) (V2 cellW cellH)

            -- 2. APLICAR TRANSPARENCIA (Tu lógica)
            let alphaValue = if entInvisible ent
                            then 128           -- 50% de opacidad
                            else 255           -- 100% de opacidad

            SDL.textureAlphaMod tex SDL.$= (fromIntegral alphaValue)

            -- 3. DIBUJAR
            SDL.copy r tex (Just srcRect) (Just destRect)

            -- 4. RESTAURAR OPACIDAD DEL TEXTURE (CRUCIAL)
            SDL.textureAlphaMod tex SDL.$= 255

            -- BARRA DE VIDA
            when (entHp ent < entMaxHp ent || entAggro ent) $ do
                drawHealthBar r screenPos (entHp ent) (entMaxHp ent)

drawTitleScreen :: SDL.Renderer -> M.Map String SDL.Texture -> Int -> Game ()
drawTitleScreen r texs sel = do
    case M.lookup "background" texs of
        Just bgTex -> SDL.copy r bgTex Nothing Nothing
        Nothing -> do
            SDL.rendererDrawColor r SDL.$= V4 0 0 50 255
            SDL.clear r
    return()

-- NUEVO: Dibujar Pantalla de Game Over
drawGameOverScreen :: SDL.Renderer -> M.Map String SDL.Texture -> Game ()
drawGameOverScreen r texs = do
    case M.lookup "gameover" texs of
        Just bgTex -> SDL.copy r bgTex Nothing Nothing
        Nothing -> do
            SDL.rendererDrawColor r SDL.$= V4 50 0 0 255 -- Fondo rojo oscuro si falla
            SDL.clear r
    return()

    -- src/Render.hs (Reemplazar la función render completa)

render :: Game ()
render = do
    st <- get
    mode <- gets gameMode
    let r = renderer st
    let res = resources st

    -- 1. CASO PRINCIPAL: MANEJO DEL MODO DE JUEGO
    case mode of
        TitleScreen -> do
            drawTitleScreen r (rTextures res) (menuSelection st)
            SDL.present r

        GameOver -> do
            drawGameOverScreen r (rTextures res)
            SDL.present r

        Playing -> do
            let texs = rTextures res
            let center = V2 (windowW `div` 2) (windowH `div` 2)
            let pCenter = V2 (screenSize `div` 2) (screenSize `div` 2)
            let cameraOffset = entPos (player st) ^-^ center ^+^ pCenter

            SDL.rendererDrawColor r SDL.$= V4 15 15 20 255
            SDL.clear r

            -- 2. RENDERIZADO DE MAPA E ÍTEMS (LÓGICA UNIFICADA)
            case (M.lookup "dungeon" texs, M.lookup "items" texs) of
                (Just texDungeon, Just texItems) -> do

                    renderLayer r texDungeon mapaSuelo cameraOffset

                    -- INYECCIÓN DE TU LÓGICA DE RENDERIZADO DE ÍTEMS
                    forM_ (mapItems st) $ \item -> do
                        unless (itemObtained item) $ do
                            renderItem r texItems item cameraOffset -- Usa texItems

                    -- Las entidades usan texDungeon (asumiendo que los héroes están allí)
                    forM_ (enemies st) $ \e -> renderEntity r texs e cameraOffset
                    renderEntity r texs (player st) cameraOffset

                _ -> return ()

            renderLog r (rFont res) (gameLog st) -- Lógica de Log (del compañero)
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
                        when (sx > -screenSize && sx < windowW && sy > -screenSize && sy < windowH) $ do
                            let destRect = SDL.Rectangle (P screenPos) (V2 screenSize screenSize)
                            SDL.copy r tex (Just srcRect) (Just destRect)

itemTileID :: ItemType -> Int
itemTileID PotionFuerza     = 20
itemTileID PotionInvisibilidad = 4
itemTileID PotionVelocidad  = 19
itemTileID PotionVeneno     = 32
itemTileID _ = -1

renderItem :: SDL.Renderer -> SDL.Texture -> Item -> V2 CInt -> Game ()
renderItem r tex item cameraOffset = do
    let tileID = itemTileID (itemType item)
    case getItemTileRect tileID of
        Nothing -> return ()
        Just srcRect -> do
            let screenPos = itemPos item ^-^ cameraOffset
            let destRect = SDL.Rectangle (P screenPos) (V2 screenSize screenSize)
            SDL.copy r tex (Just srcRect) (Just destRect)
