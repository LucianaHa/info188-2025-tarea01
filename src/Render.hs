module Render where

import qualified SDL
import qualified SDL.Font
import Linear (V2(..), V4(..), (^-^), (^+^))
import Linear.Affine (Point(..))
import Control.Monad.State
import Control.Monad (when, forM_, unless, forM)
import qualified Data.Map as M
import qualified Data.Text as T
import Foreign.C.Types (CInt, CDouble)
import Data.Word (Word8, Word32)

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


isPlayerClass :: Clase -> Bool
isPlayerClass Guerrero = True
isPlayerClass Mago     = True
isPlayerClass Asesino  = True
isPlayerClass _        = False

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
-- (Tus imports y funciones auxiliares siguen igual...)

-- Función para dibujar texto simple
renderText :: SDL.Renderer -> SDL.Font.Font -> Int -> Int -> String -> V4 Word8 -> Game ()
renderText r font x y content color = do
    let text = T.pack content
    surface <- SDL.Font.blended font color text
    dims <- SDL.surfaceDimensions surface
    texture <- SDL.createTextureFromSurface r surface
    let (V2 w h) = dims
    let destRect = SDL.Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral w) (fromIntegral h))
    SDL.copy r texture Nothing (Just destRect)
    SDL.destroyTexture texture
    SDL.freeSurface surface

-- DIBUJAR ESTADÍSTICAS (HUD)

renderMissionMessage :: SDL.Renderer -> Maybe SDL.Font.Font -> Word32 -> Word32 -> Game ()
renderMissionMessage r Nothing _ _ = return ()
renderMissionMessage r (Just font) currentTicks startTicks = do
    -- Calculamos si han pasado menos de 5 segundos (5000 ms)
    let timePassed = currentTicks - startTicks
    
    when (timePassed < 5000) $ do
        let message = "Sube Nivel y Derrota a la Vaca"
        let color = V4 255 255 0 255 -- Amarillo
        let shadow = V4 0 0 0 255    -- Negro

        -- Renderizamos el texto (usando tu librería de font)
        textSurface <- SDL.Font.blended font color (T.pack message)
        textDims <- SDL.surfaceDimensions textSurface
        textTex <- SDL.createTextureFromSurface r textSurface
        
        -- Sombra
        shadowSurface <- SDL.Font.blended font shadow (T.pack message)
        shadowTex <- SDL.createTextureFromSurface r shadowSurface

        let (V2 w h) = textDims
        
        -- Centrar en pantalla (1280x720)
        let centerX = (1280 - fromIntegral w) `div` 2
        let centerY = (720 - fromIntegral h) `div` 2 - 100 -- Un poco más arriba del centro

        let rectShadow = SDL.Rectangle (P (V2 (centerX + 3) (centerY + 3))) (V2 (fromIntegral w) (fromIntegral h))
        let rectText   = SDL.Rectangle (P (V2 centerX centerY)) (V2 (fromIntegral w) (fromIntegral h))

        -- Dibujar sombra y luego texto
        SDL.copy r shadowTex Nothing (Just rectShadow)
        SDL.copy r textTex Nothing (Just rectText)

        -- Limpiar
        SDL.destroyTexture textTex
        SDL.freeSurface textSurface
        SDL.destroyTexture shadowTex
        SDL.freeSurface shadowSurface


renderHUD :: SDL.Renderer -> Maybe SDL.Font.Font -> Entity -> Game ()
renderHUD r Nothing _ = return ()
renderHUD r (Just font) pj = do
    -- Configuración visual base
    let startX = 20
    let startY = 20
    let lineHeight = 20
    
    -- Colores
    let colorTexto = V4 255 255 255 255   -- Blanco
    let colorSombra = V4 0 0 0 255        -- Negro (sombra)
    let colorControls = V4 100 255 255 255 -- Cyan (para diferenciar los controles)

    -- ===========================================
    -- 1. ESTADÍSTICAS (HP, XP, LEVEL)
    -- ===========================================
    let txtHp  = "VIDA: " ++ show (entHp pj) ++ " / " ++ show (entMaxHp pj)
    let txtLvl = "NIVEL: " ++ show (entLevel pj)
    let txtXp  = "XP: " ++ show (entXp pj) ++ " / " ++ show (entNextLevel pj)
    
    -- Helper para dibujar línea con sombra
    let drawLine y text color = do
            renderText r font (startX + 2) (y + 2) text colorSombra -- Sombra
            renderText r font startX y text color -- Texto principal

    drawLine (startY + 0 * lineHeight) txtHp colorTexto
    drawLine (startY + 1 * lineHeight) txtLvl colorTexto
    drawLine (startY + 2 * lineHeight) txtXp colorTexto

    -- ===========================================
    -- 2. CONTROLES (MÁS ABAJO)
    -- ===========================================
    
    -- Dejamos un espacio (gap) de 40px después de las stats
    let controlsY = startY + (3 * lineHeight) + 20 

    -- Dibujamos los textos de ayuda
    drawLine (controlsY + 0 * lineHeight) "CONTROLES:" colorControls
    drawLine (controlsY + 1 * lineHeight) "[Q] Ataque Normal" colorControls
    drawLine (controlsY + 2 * lineHeight) "[W] Ataque en Area" colorControls
    drawLine (controlsY + 3 * lineHeight) "Daño por espalda x1.5" colorControls



renderEntity :: SDL.Renderer -> M.Map String SDL.Texture -> Entity -> V2 CInt -> Game ()
renderEntity r texs ent cameraOffset = do
    let entClass' = entClass ent
    let tileScreenPos = entPos ent ^-^ cameraOffset

    -- =======================================================
    -- BLOQUE 1: PERSONAJE (96px, Alineado al fondo)
    -- =======================================================
    
    let offsetX = (screenSize - entityRenderSize) `div` 2
    let offsetY = screenSize - entityRenderSize -- Pies en la base (-32px Y)
    
    let heroDrawPos = tileScreenPos + V2 offsetX offsetY
    let destRect = SDL.Rectangle (P heroDrawPos) (V2 entityRenderSize entityRenderSize)

    let (texKey, frameIndex) = case entClass' of
                                    Guerrero -> ("hero", entAnimFrame ent)
                                    Orco     -> ("ogre", entAnimFrame ent)
                                    Zombie   -> ("zombie", entAnimFrame ent)
                                    Vaca     -> ("cow", entAnimFrame ent)
                                    _        -> ("hero", entAnimFrame ent)

    case M.lookup texKey texs of
        Nothing -> return ()
        Just tex -> do
            let cellSize = case texKey of "cow" -> 48; _ -> 32
            let row = case entDir ent of Abajo -> 0; Izquierda -> 1; Arriba -> 2; Derecha -> 3
            let col = fromIntegral (entAnimFrame ent)
            let srcRect = SDL.Rectangle (P (V2 (col * cellSize) (row * cellSize))) (V2 cellSize cellSize)

            let alphaValue = if entInvisible ent then 128 else 255
            SDL.textureAlphaMod tex SDL.$= (fromIntegral alphaValue)
            SDL.copy r tex (Just srcRect) (Just destRect)
            SDL.textureAlphaMod tex SDL.$= 255

    -- =======================================================
    -- BLOQUE 2: ESCUDO (Centrado Perfecto y Estable)
    -- =======================================================
    
    when (isPlayerClass entClass' && not (entDead ent)) $ do
        case M.lookup "shield" texs of
            Nothing -> return ()
            Just texShield -> do
                
                -- Dimensiones: 100 ancho x 80 alto (Proporción correcta)
                let shieldW = 100
                let shieldH = 80
                let shieldSize = V2 shieldW shieldH
                
                -- Ángulo de rotación
                let angle :: CDouble
                    angle = case entDir ent of
                        Arriba    -> 0
                        Derecha   -> 90
                        Abajo     -> 180
                        Izquierda -> 270

                -- POSICIÓN FIJA (Sin 'case' para offsets manuales)
                
                -- 1. Centramos el escudo (100x80) sobre el sprite del héroe (96x96)
                let centerOffsetX = (entityRenderSize - shieldW) `div` 2
                let centerOffsetY = (entityRenderSize - shieldH) `div` 2
                

                let verticalShift = case entDir ent of
                        Arriba -> 16 
                        Abajo -> 24
                        _     -> 20
                -- Posición final constante
                let finalShieldPos = heroDrawPos + V2 centerOffsetX (centerOffsetY + verticalShift)
                
                let destRectShield = SDL.Rectangle (P finalShieldPos) shieldSize

                -- Renderizado
                let alphaValue = if entInvisible ent then 60 else 160

                SDL.textureAlphaMod texShield SDL.$= (fromIntegral alphaValue)

                SDL.copyEx r texShield Nothing (Just destRectShield) angle Nothing (SDL.V2 False False)
                
                SDL.textureAlphaMod texShield SDL.$= 255

    -- =======================================================
    -- BLOQUE 3: BARRA DE VIDA
    -- =======================================================
    
    when (entHp ent < entMaxHp ent || entAggro ent) $ do
        let barPos = tileScreenPos + V2 0 (-106)
        drawHealthBar r barPos (entHp ent) (entMaxHp ent)

    -- =======================================================
    -- BLOQUE 4: DEBUG DE ATAQUE (CAJAS ROJAS)
    -- =======================================================
    
    let atkType = entAttackType ent
    
    -- Solo dibujar si hay un ataque activo
    when (atkType /= NoAttack) $ do
        
        -- 1. CONFIGURAR DIBUJO ROJO TRANSPARENTE
        -- Guardamos el modo de mezcla anterior para no romper nada
        oldBlend <- SDL.get (SDL.rendererDrawBlendMode r)
        
        -- Activamos mezcla (Alpha Blending) para que sea transparente
        SDL.rendererDrawBlendMode r SDL.$= SDL.BlendAlphaBlend
        
        -- Color Rojo: R=255, G=0, B=0, Alpha=150 (Semitransparente)
        SDL.rendererDrawColor r SDL.$= V4 255 0 0 150

        -- 2. CALCULAR DÓNDE DIBUJAR LA CAJA
        case atkType of
            
            -- CASO Q: Cuadrado frente al jugador
            AtkNormal -> do
                let offset = case entDir ent of
                        Arriba    -> V2 0 (-screenSize) -- Una casilla arriba (-64)
                        Abajo     -> V2 0 screenSize    -- Una casilla abajo (+64)
                        Izquierda -> V2 (-screenSize) 0
                        Derecha   -> V2 screenSize 0
                
                -- tileScreenPos es la esquina de TU casilla. Le sumamos el offset.
                let attackPos = tileScreenPos + offset
                
                -- Dibujamos un cuadrado del tamaño de una casilla (64x64)
                let rect = SDL.Rectangle (P attackPos) (V2 screenSize screenSize)
                SDL.fillRect r (Just rect)

            -- CASO W: Área grande centrada
            AtkArea -> do
                -- Queremos un área de radio 100 (aprox 3 casillas de ancho total)
                let areaSize = 200 -- 100 de radio x 2
                
                -- Calculamos el centro para que quede sobre el jugador
                -- (64 - 200) / 2 = -68. 
                let offset = (screenSize - areaSize) `div` 2
                let attackPos = tileScreenPos + V2 offset offset
                
                let rect = SDL.Rectangle (P attackPos) (V2 areaSize areaSize)
                SDL.fillRect r (Just rect)
            
            _ -> return ()

        -- 3. RESTAURAR ESTADO (IMPORTANTE)
        -- Volvemos al modo de mezcla normal y color blanco/negro por defecto
        SDL.rendererDrawBlendMode r SDL.$= oldBlend
        SDL.rendererDrawColor r SDL.$= V4 0 0 0 255


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
    currentTicks <- SDL.ticks
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
            renderHUD r (rFont res) (player st)
            renderLog r (rFont res) (gameLog st) -- Lógica de Log
            renderMissionMessage r (rFont res) currentTicks (gameStartTime st)
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
