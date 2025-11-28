module Render where

import qualified SDL
import qualified SDL.Font
import Linear (V2(..), V4(..), (^-^), (^+^))
import Linear.Affine (Point(..))
import Control.Monad.State
import Control.Monad (when, forM_)
import qualified Data.Map as M
import qualified Data.Text as T
import Foreign.C.Types (CInt)

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

-- NUEVA LÓGICA DE RENDERIZADO DE LOG (UI)
renderLog :: SDL.Renderer -> Maybe SDL.Font.Font -> [String] -> Game ()
renderLog _ Nothing _ = return ()
renderLog r (Just font) logs = do
    -- 1. Configuración de Estilo
    let colorTexto = V4 255 255 255 255 -- Blanco
    let colorFondo = V4 0 0 0 180       -- Negro con Alpha (180/255 = ~70% opacidad)

    -- Definimos el área del panel en la esquina inferior derecha
    let panelW = 500  -- Ancho del recuadro
    let panelH = 120  -- Alto del recuadro (para unas 5-6 líneas)
    let margin = 20   -- Separación del borde de la ventana

    let baseX = fromIntegral windowW - panelW - margin
    let baseY = fromIntegral windowH - panelH - margin

    -- 2. Dibujar Fondo Traslúcido
    -- Necesitamos activar BlendAlphaBlend para que el canal Alpha (transparencia) funcione
    -- CORRECCIÓN: Usamos SDL.BlendAlphaBlend en lugar de BlendModeBlend
    SDL.rendererDrawBlendMode r SDL.$= SDL.BlendAlphaBlend
    SDL.rendererDrawColor r SDL.$= colorFondo

    let bgRect = SDL.Rectangle (P (V2 baseX baseY)) (V2 panelW panelH)
    SDL.fillRect r (Just bgRect)

    -- Restauramos BlendMode a None (opcional, pero buena práctica para no afectar otros dibujos)
    -- CORRECCIÓN: Usamos SDL.BlendNone
    SDL.rendererDrawBlendMode r SDL.$= SDL.BlendNone

    -- 3. Dibujar Texto
    let recentLogs = take 6 logs -- Mostramos hasta 6 mensajes
    let lineHeight = 18          -- Espacio vertical entre líneas (fuente 12 + margen)
    let textMarginX = 10         -- Margen interno del texto
    let textMarginY = 10

    forM_ (zip [0..] recentLogs) $ \(i, msg) -> do
        let text = T.pack msg
        surface <- SDL.Font.blended font colorTexto text
        dims <- SDL.surfaceDimensions surface

        -- CORRECCIÓN DEL ERROR: Usamos pattern matching
        let (V2 w h) = dims
        let textW = fromIntegral w
        let textH = fromIntegral h

        -- Posición del texto dentro del recuadro (de arriba hacia abajo o viceversa)
        -- Aquí los dibujamos de arriba hacia abajo dentro del recuadro
        let posX = baseX + textMarginX
        let posY = baseY + textMarginY + (fromIntegral i * lineHeight)

        texture <- SDL.createTextureFromSurface r surface
        let destRect = SDL.Rectangle (P (V2 posX posY)) (V2 textW textH)

        SDL.copy r texture Nothing (Just destRect)
        SDL.destroyTexture texture
        SDL.freeSurface surface

renderEntity :: SDL.Renderer -> SDL.Texture -> Entity -> V2 CInt -> Game ()
renderEntity r tex ent cameraOffset = do
    let charIndex = characterIndexFromClass (entClass ent)
    let srcX = charIndex * heroSize
    let srcY = heroStartY

    let srcRect = SDL.Rectangle (P (V2 srcX srcY)) (V2 heroSize heroSize)
    let screenPos = entPos ent ^-^ cameraOffset
    let destRect = SDL.Rectangle (P screenPos) (V2 screenSize screenSize)

    SDL.copy r tex (Just srcRect) (Just destRect)

    when (entHp ent < entMaxHp ent || entAggro ent) $ do
        drawHealthBar r screenPos (entHp ent) (entMaxHp ent)

render :: Game ()
render = do
    st <- get
    let r = renderer st
    let res = resources st

    let center = V2 (windowW `div` 2) (windowH `div` 2)
    let pCenter = V2 (screenSize `div` 2) (screenSize `div` 2)
    let cameraOffset = entPos (player st) ^-^ center ^+^ pCenter

    SDL.rendererDrawColor r SDL.$= V4 15 15 20 255
    SDL.clear r

    case M.lookup "dungeon" (rTextures res) of
        Just texDungeon -> do
            renderLayer r texDungeon mapaSuelo cameraOffset
            forM_ (enemies st) $ \e -> renderEntity r texDungeon e cameraOffset
            renderEntity r texDungeon (player st) cameraOffset
        Nothing -> return ()

    -- RENDERIZAR INTERFAZ
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

                        when (sx > -screenSize && sx < windowW && sy > -screenSize && sy < windowH) $ do
                            let destRect = SDL.Rectangle (P screenPos) (V2 screenSize screenSize)
                            SDL.copy r tex (Just srcRect) (Just destRect)
