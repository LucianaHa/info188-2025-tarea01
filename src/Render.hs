module Render where

import qualified SDL
import qualified SDL.Font
import Linear (V2(..), V3(..), V4(..), (^-^), (^+^))
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
characterIndexFromClass Hero    = 0
characterIndexFromClass Paladin = 0
characterIndexFromClass Bruja   = 0
characterIndexFromClass Chamana = 0
characterIndexFromClass Orco    = 3
characterIndexFromClass Esqueleto = 4
characterIndexFromClass Zombie    = 4
characterIndexFromClass Vaca      = 4

getTileRect :: Int -> Maybe (SDL.Rectangle CInt)
getTileRect id
    | id == 50  = Just (SDL.Rectangle (P (V2 (9 * tileSizeSource) 0)) (V2 tileSizeSource tileSizeSource)) -- <--- NUEVO: Puerta
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
isPlayerClass Hero    = True
isPlayerClass Paladin = True
isPlayerClass Bruja   = True
isPlayerClass Chamana = True
isPlayerClass _       = False

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


renderMissionMessage :: SDL.Renderer -> Maybe SDL.Font.Font -> Word32 -> Word32 -> Game ()
renderMissionMessage r Nothing _ _ = return ()
renderMissionMessage r (Just font) currentTicks startTicks = do
    -- Calculamos si han pasado menos de 5 segundos (5000 ms)
    let timePassed = currentTicks - startTicks

    when (timePassed < 5000) $ do
        let message = "Sube Nivel y Derrota a la Vaca"
        let color = V4 255 255 0 255 -- Amarillo
        let shadow = V4 0 0 0 255    -- Negro

        -- Renderizamos el texto
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

    -- =========================================================
    -- 1. CONFIGURACIÓN DE TAMAÑO Y AJUSTE FINO (NUDGE)
    -- =========================================================
    -- Definimos (Tamaño, Ajuste X, Ajuste Y) para cada clase.
    -- Ajuste X > 0: Mueve a la derecha.
    -- Ajuste Y > 0: Mueve hacia abajo.

    let (renderSize, nudgeX, nudgeY) = case entClass' of
            -- RATA: Pequeña (48px), centrada
            Rata    -> (48,  8, 0)

            -- VACA: Gigante (128px), centrada
            Vaca    -> (128, 0, 0)

            -- HEROE: Estándar (96px).
            -- Si se ve muy a la izquierda, SUBE el valor de nudgeX (ej. 10, 16)
            Hero    -> (96,  0, 0)

            -- Otros (Usan valores por defecto)
            _       -> (96,  0, 0)

    -- =========================================================
    -- 2. CALCULAR POSICIÓN
    -- =========================================================

    -- Centrado Automático Matemático
    let autoOffsetX = (screenSize - renderSize) `div` 2
    let autoOffsetY = screenSize - renderSize

    -- Posición Final = Base + Automático + ajuste manual
    let finalX = autoOffsetX + nudgeX
    let finalY = autoOffsetY + nudgeY

    let heroDrawPos = tileScreenPos + V2 finalX finalY
    let destRect = SDL.Rectangle (P heroDrawPos) (V2 renderSize renderSize)

    -- =========================================================
    -- 3. SELECCIONAR TEXTURA Y RECORTE
    -- =========================================================
    let (texKey, _) = case entClass' of
            Hero    -> ("hero", entAnimFrame ent)
            Paladin -> ("paladin", entAnimFrame ent)
            Bruja   -> ("bruja", entAnimFrame ent)
            Chamana -> ("chamana", entAnimFrame ent)
            Orco    -> ("ogre", entAnimFrame ent)
            Zombie  -> ("zombie", entAnimFrame ent)
            Vaca    -> ("cow", entAnimFrame ent)
            Rata    -> ("rat", entAnimFrame ent)
            _       -> ("hero", entAnimFrame ent)

    case M.lookup texKey texs of
        Nothing -> return ()
        Just tex -> do

            let cellSize = case texKey of
                    "cow" -> 48
                    "rat" -> 16
                    _     -> 32

            let row = case entDir ent of Abajo -> 0; Izquierda -> 1; Arriba -> 2; Derecha -> 3
            let col = fromIntegral (entAnimFrame ent)
            let srcRect = SDL.Rectangle (P (V2 (col * cellSize) (row * cellSize))) (V2 cellSize cellSize)

            SDL.textureBlendMode tex SDL.$= SDL.BlendAlphaBlend
            let alphaValue = if entInvisible ent then 128 else 255
            SDL.textureAlphaMod tex SDL.$= (fromIntegral alphaValue)

            SDL.copy r tex (Just srcRect) (Just destRect)
            SDL.textureAlphaMod tex SDL.$= 255

    -- =========================================================
    -- 4. ESCUDO (Ajustado a la nueva posición)
    -- =========================================================
    when (isPlayerClass entClass' && not (entDead ent)) $ do
        case M.lookup "shield" texs of
            Nothing -> return ()
            Just texShield -> do
                let shieldW = 100; shieldH = 80
                let shieldSize = V2 shieldW shieldH
                let angle = case entDir ent of Arriba -> 0; Derecha -> 90; Abajo -> 180; Izquierda -> 270

                -- Se uso heroDrawPos para que el escudo siga al cuerpo
                -- Centramos el escudo (100x80) sobre el render del personaje (renderSize)

                let shieldCenterX = (renderSize - shieldW) `div` 2
                let shieldCenterY = (renderSize - shieldH) `div` 2
                let verticalShift = case entDir ent of
                        Abajo  -> 24
                        Arriba -> 16
                        _      -> 16
                let finalShieldPos = heroDrawPos + V2 shieldCenterX (shieldCenterY + verticalShift)
                let destRectShield = SDL.Rectangle (P finalShieldPos) shieldSize

                let alphaVal = if entInvisible ent then 60 else 160
                SDL.textureAlphaMod texShield SDL.$= (fromIntegral alphaVal)
                SDL.copyEx r texShield Nothing (Just destRectShield) angle Nothing (SDL.V2 False False)
                SDL.textureAlphaMod texShield SDL.$= 255

    -- =========================================================
    -- 5. BARRA DE VIDA
    -- =========================================================
    when (entHp ent < entMaxHp ent || entAggro ent) $ do
        -- La barra flota un poco más arriba de la cabeza
        let barPos = heroDrawPos + V2 0 (-10)

        -- Si es rata, barra pequeña (32), si no, normal (64)
        let barWidth = if entClass' == Rata then 32 else 64

        -- Centrar la barra respecto al sprite
        let barOffsetX = (renderSize - barWidth) `div` 2

        drawHealthBarCustom r (barPos + V2 barOffsetX 0) barWidth (entHp ent) (entMaxHp ent)

    let atkType = entAttackType ent

    when (atkType /= NoAttack) $ do
        case M.lookup "fx_blade" texs of
            Nothing -> return ()
            Just texBlade -> do

                -- 1. FRAME DE ANIMACIÓN
                -- Usamos el frame del personaje para sincronizar (0, 1, 2)
                let frame = fromIntegral (entAnimFrame ent) `mod` 3
                let frameW = 88 -- Ancho de cada cuadro en la hoja (264 / 3)

                -- 2. SELECCIONAR FILA (Y) SEGÚN TIPO DE ATAQUE
                -- Ajusta estos valores 'srcY' si ves que recorta mal la hoja
                let (srcY, frameH, isSpin) = case atkType of
                        AtkNormal -> (240, 30, False)   -- Fila 0: Estocada / Corte
                        AtkArea   -> (360, 88, True)  -- Fila media: Giro circular
                        _         -> (0, 0, False)

                let srcRect = SDL.Rectangle (P (V2 (frame * frameW) srcY)) (V2 frameW frameH)

                -- 3. CALCULAR POSICIÓN Y ROTACIÓN
                let (destRect, rotAngle) = if isSpin
                    then
                        -- CASO W: GIRO (Centrado en el personaje)
                        let size = 180 -- Tamaño grande para el área
                            offset = (entityRenderSize - size) `div` 2
                            pos = heroDrawPos + V2 offset offset
                        in (SDL.Rectangle (P pos) (V2 size size), 0)
                    else
                        -- CASO Q: GOLPE (Frente al personaje)
                        let sizeW = 96
                            sizeH = 70

                            -- Calculamos el centro del cuerpo
                            centerX = (entityRenderSize - sizeW) `div` 2
                            centerY = (entityRenderSize - sizeH) `div` 2
                            centerPos = heroDrawPos + V2 centerX centerY

                            -- Se empuja el efecto hacia donde mira
                            offsetDir = case entDir ent of
                                     Arriba    -> V2 0 (-60)
                                     Abajo     -> V2 0 60
                                     Derecha   -> V2 55 20
                                     Izquierda -> V2 (-55) 0
                            
                            finalPos = centerPos + offsetDir

                            -- Rotamos el sprite para que apunte bien
                            ang = case entDir ent of
                                    Arriba -> 270; Derecha -> 0; Abajo -> 90; Izquierda -> 180
                        in (SDL.Rectangle (P finalPos) (V2 sizeW sizeH), ang)

                -- 4. DIBUJAR CON BRILLO (Additive blending)
                SDL.textureBlendMode texBlade SDL.$= SDL.BlendAdditive
                SDL.copyEx r texBlade (Just srcRect) (Just destRect) rotAngle Nothing (SDL.V2 False False)
                SDL.textureBlendMode texBlade SDL.$= SDL.BlendAlphaBlend


drawHealthBarCustom :: SDL.Renderer -> V2 CInt -> CInt -> Int -> Int -> Game ()
drawHealthBarCustom r (V2 x y) width hp maxHp = do
    let height = 6
    let barBg = SDL.Rectangle (P (V2 x y)) (V2 width height)
    let pct = fromIntegral hp / fromIntegral maxHp :: Float
    let greenW = floor (fromIntegral width * pct) :: CInt
    let barFg = SDL.Rectangle (P (V2 x y)) (V2 greenW height)

    SDL.rendererDrawColor r SDL.$= V4 255 0 0 255
    SDL.fillRect r (Just barBg)
    SDL.rendererDrawColor r SDL.$= V4 0 255 0 255
    SDL.fillRect r (Just barFg)

drawTitleScreen :: SDL.Renderer -> Maybe SDL.Font.Font -> M.Map String SDL.Texture -> Entity -> Game ()
drawTitleScreen r Nothing texs _ = return ()
drawTitleScreen r (Just font) texs pj = do
    -- Fondo
    case M.lookup "background" texs of
        Just bgTex -> SDL.copy r bgTex Nothing Nothing
        Nothing -> SDL.clear r

    -- Texto de Selección
    let claseNombre = show (entClass pj)
    let infoTxt = "Clase actual: " ++ claseNombre ++ " (Usa 1-4 para cambiar)"
    let color = V4 0 0 0 0

    -- Renderizamos texto en la parte superior
    renderText r font 50 30 infoTxt color

    -- Mostrar stats breves
    let statsTxt = "HP: " ++ show (entMaxHp pj) ++ " | ATK: " ++ show (entMinAtk pj) ++ "-" ++ show (entMaxAtk pj) ++ " | SPD: " ++ show (entSpeed pj)
    renderText r font 50 50 statsTxt color

drawGameOverScreen :: SDL.Renderer -> M.Map String SDL.Texture -> Game ()
drawGameOverScreen r texs = do
    case M.lookup "gameover" texs of
        Just bgTex -> SDL.copy r bgTex Nothing Nothing
        Nothing -> do
            SDL.rendererDrawColor r SDL.$= V4 50 0 0 255 -- Fondo rojo oscuro si falla
            SDL.clear r
    return ()


drawWinScreen :: SDL.Renderer -> Game ()
drawWinScreen r = do
    SDL.rendererDrawColor r SDL.$= V4 0 200 0 255 -- Fondo Verde Victoria
    SDL.clear r


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
            drawTitleScreen r (rFont res) (rTextures res) (player st)
            SDL.present r

        GameWon -> do
            drawWinScreen r
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

                    renderLayer r texDungeon (currentMap st) cameraOffset

                    forM_ (mapItems st) $ \item -> do
                        unless (itemObtained item) $ do
                            renderItem r texItems item cameraOffset

                    forM_ (enemies st) $ \e -> renderEntity r texs e cameraOffset
                    renderEntity r texs (player st) cameraOffset

                _ -> return ()
            renderHUD r (rFont res) (player st)
            renderLog r (rFont res) (gameLog st)
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
itemTileID Llave            = 29
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
