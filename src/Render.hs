module Render where

import qualified SDL
import qualified SDL.Font -- Necesario para dibujar texto
import Linear (V2(..), V4(..), (^-^), (^+^))
import Linear.Affine (Point(..))
import Control.Monad.State
import Control.Monad (when, forM_)
import qualified Data.Map as M
import qualified Data.Text as T -- Necesario para dibujar texto
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

-- CÓDIGO COLABORADOR: NUEVA FUNCIÓN: Renderizar Texto (Log de juego)
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

    -- 2. Dibujar Fondo Traslúcido
    SDL.rendererDrawBlendMode r SDL.$= SDL.BlendAlphaBlend
    SDL.rendererDrawColor r SDL.$= colorFondo

    let bgRect = SDL.Rectangle (P (V2 baseX baseY)) (V2 panelW panelH)
    SDL.fillRect r (Just bgRect)

    -- Restauramos BlendMode a None
    SDL.rendererDrawBlendMode r SDL.$= SDL.BlendNone

    -- 3. Dibujar Texto
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


-- NUEVA FUNCION AUXILIAR: Obtiene la fila Y (srcY) del sprite del héroe
getHeroSrcY :: Direccion -> CInt
getHeroSrcY Abajo    = heroRowDown
getHeroSrcY Izquierda = heroRowLeft
getHeroSrcY Derecha  = heroRowRight
getHeroSrcY Arriba   = heroRowUp

-- Renderizado de entidades (Ajustado para usar heroSize=32)
renderEntity :: SDL.Renderer -> AssetManager -> Entity -> V2 CInt -> Game ()
renderEntity r texs ent cameraOffset = do
    let entClass' = entClass ent
    let screenPos = entPos ent ^-^ cameraOffset
    
    -- Lógica para determinar si la entidad es un personaje principal (Héroe u Ogro)
    let isMainCharacter = entClass' == Guerrero || entClass' == Orco
    
    -- AJUSTE VISUAL CLAVE: Escalar al 1.125x (72 píxeles) si es un personaje principal
    let destSize = if isMainCharacter 
                   then screenSize * 9 `div` 8 -- 64 * 1.125 = 72px
                   else screenSize -- 64px para Esqueletos/Mago/Mapa
    
    let destRect = SDL.Rectangle (P screenPos) (V2 destSize destSize) 

    -- 1. SELECCIONAR LA TEXTURA Y FRAME
    let (texKey, frameIndex) = case entClass' of
                                    Guerrero -> ("hero", entAnimFrame ent)
                                    Orco     -> ("ogre", entAnimFrame ent) 
                                    _        -> ("dungeon", fromIntegral (characterIndexFromClass entClass')) 
    
    -- Determina el tamaño de la fuente (src)
    let spriteSize = if texKey == "dungeon" then tileSizeSource else heroSize

    case M.lookup texKey texs of
        Nothing -> return () 

        Just tex -> do
            let srcRect = if texKey == "dungeon"
                    then -- ENEMIGOS BASE (16x16, sin animación de movimiento)
                        let 
                            srcX = fromIntegral frameIndex * spriteSize
                            srcY = heroStartY 
                        in SDL.Rectangle (P (V2 srcX srcY)) (V2 spriteSize spriteSize)
                    else -- HÉROE U OGRO (32x32, con animación)
                        let 
                            srcX = fromIntegral (entAnimFrame ent) * heroSize
                            srcY = getHeroSrcY (entDir ent) 
                        in SDL.Rectangle (P (V2 srcX srcY)) (V2 heroSize heroSize)

            -- DIBUJAR SPRITE
            SDL.copy r tex (Just srcRect) (Just destRect)
            
            -- BARRA DE VIDA 
            when (entHp ent < entMaxHp ent || entAggro ent) $ do
                drawHealthBar r screenPos (entHp ent) (entMaxHp ent)


-- NUEVO: Dibujar Pantalla de Título con Botones (NECESARIO para línea 103)
drawTitleScreen :: SDL.Renderer -> AssetManager -> Int -> Game ()
drawTitleScreen r texs sel = do
    case M.lookup "background" texs of
        Just bgTex -> SDL.copy r bgTex Nothing Nothing 
        Nothing -> do 
            SDL.rendererDrawColor r SDL.$= V4 0 0 50 255 
            SDL.clear r

    SDL.present r


-- Render Principal (Asegura que el mapa usa la textura correcta)
render :: Game ()
render = do
    st <- get
    mode <- gets gameMode
    let r = renderer st
    let res = resources st -- Usamos 'resources' para acceder al AssetManager y Font

    case mode of
        TitleScreen -> 
            drawTitleScreen r (rTextures res) (menuSelection st)
        
        Playing -> do
            let texs = rTextures res -- Usamos el AssetManager del recurso
            let center = V2 (windowW `div` 2) (windowH `div` 2)
            let pCenter = V2 (screenSize `div` 2) (screenSize `div` 2)
            let cameraOffset = entPos (player st) ^-^ center ^+^ pCenter

            -- FONDO OSCURO DE MAZMORRA 
            SDL.rendererDrawColor r SDL.$= V4 15 15 20 255 
            SDL.clear r

            -- RENDERIZADO DEL MAPA
            case M.lookup "dungeon" texs of 
                Just texDungeon -> do
                    renderLayer r texDungeon mapaSuelo cameraOffset
                    
                    -- Renderizamos entidades usando el AssetManager completo
                    forM_ (enemies st) $ \e -> renderEntity r texs e cameraOffset
                    renderEntity r texs (player st) cameraOffset
                    
                Nothing -> return ()
            
            -- DIBUJAR LOG (UI)
            renderLog r (rFont res) (gameLog st)

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