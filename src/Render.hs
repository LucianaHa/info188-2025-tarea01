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
characterIndexFromClass Zombie    = 4 -- Placeholder seguro (reutilizamos índice o uno nuevo si existiera en tileset)

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

-- Helper seguro para queryTexture
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
                                    _        -> ("dungeon", fromIntegral (characterIndexFromClass entClass'))

    let spriteSize = if texKey == "dungeon" then tileSizeSource else heroSize

    case M.lookup texKey texs of
        Nothing -> return ()

        Just tex -> do
            srcRect <- if texKey == "dungeon"
                then
                    let
                        srcX = fromIntegral frameIndex * spriteSize
                        srcY = heroStartY
                    in return $ SDL.Rectangle (P (V2 srcX srcY)) (V2 spriteSize spriteSize)
                else do
                    -- Cálculo dinámico y alineado al fondo
                    (texW, texH) <- queryTextureSafe tex

                    let cols = 4
                    let rows = 4

                    let cellW = fromIntegral texW `div` cols
                    let cellH = fromIntegral texH `div` rows

                    let row = case entDir ent of
                                Abajo     -> 0
                                Izquierda -> 1
                                Arriba    -> 2
                                Derecha   -> 3

                    let col = fromIntegral (entAnimFrame ent)

                    -- Posición en el sprite sheet
                    let srcX = col * cellW
                    let srcY = row * cellH

                    return $ SDL.Rectangle (P (V2 srcX srcY)) (V2 cellW cellH)

            SDL.copy r tex (Just srcRect) (Just destRect)

            when (entHp ent < entMaxHp ent || entAggro ent) $ do
                drawHealthBar r screenPos (entHp ent) (entMaxHp ent)

drawTitleScreen :: SDL.Renderer -> M.Map String SDL.Texture -> Int -> Game ()
drawTitleScreen r texs sel = do
    case M.lookup "background" texs of
        Just bgTex -> SDL.copy r bgTex Nothing Nothing
        Nothing -> do
            SDL.rendererDrawColor r SDL.$= V4 0 0 50 255
            SDL.clear r
    SDL.present r

render :: Game ()
render = do
    st <- get
    mode <- gets gameMode
    let r = renderer st
    let res = resources st

    case mode of
        TitleScreen ->
            drawTitleScreen r (rTextures res) (menuSelection st)

        Playing -> do
            let texs = rTextures res
            let center = V2 (windowW `div` 2) (windowH `div` 2)
            let pCenter = V2 (screenSize `div` 2) (screenSize `div` 2)
            let cameraOffset = entPos (player st) ^-^ center ^+^ pCenter

            SDL.rendererDrawColor r SDL.$= V4 15 15 20 255
            SDL.clear r

            case M.lookup "dungeon" (rTextures res) of
                Just texDungeon -> do
                    renderLayer r texDungeon mapaSuelo cameraOffset
                    forM_ (enemies st) $ \e -> renderEntity r texs e cameraOffset
                    renderEntity r texs (player st) cameraOffset
                Nothing -> return ()

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
