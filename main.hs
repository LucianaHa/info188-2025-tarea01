{-# LANGUAGE OverloadedStrings #-}

import qualified SDL
import qualified SDL.Image
import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))

import Control.Monad (unless, when, void, forM_)
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word (Word32)
import Foreign.C.Types (CInt)

-- ==========================================
-- 1. CONFIGURACIÓN DE TAMAÑOS (¡EDITA AQUÍ!)
-- ==========================================

-- TAMAÑO DEL HÉROE EN LA IMAGEN (SOURCE)
-- Tu imagen es 128 ancho / 4 frames = 32 píxeles
heroSize :: CInt
heroSize = 32

-- TAMAÑO DEL TILE EN LA IMAGEN (SOURCE)
-- Si tu tile se ve mal, cambia esto a 16
tileSizeSource :: CInt
tileSizeSource = 32 

-- TAMAÑO EN PANTALLA (DESTINATION)
-- Esto es qué tan grande se ve en el monitor (Zoom x2)
-- 32 * 2 = 64 píxeles en pantalla
screenSize :: CInt
screenSize = 64

-- MAPA (0 = Pasto, 1 = Muro/Negro)
mapaPrueba :: [[Int]]
mapaPrueba = 
    [ [0,0,0,0,1,0,0,0,0,0]
    , [0,0,0,0,1,0,0,0,0,0]
    , [0,0,1,1,1,0,0,0,0,0]
    , [0,0,1,0,0,0,0,0,0,0]
    , [0,0,1,0,0,0,0,0,0,0]
    , [0,0,0,0,0,0,0,0,0,0]
    , [0,0,0,0,0,0,0,0,0,0]
    ]

-- ==========================================
-- 2. TIPOS DE DATOS
-- ==========================================

data Direccion = Abajo | Izquierda | Derecha | Arriba 
    deriving (Show, Enum, Eq)

type AssetManager = Map String SDL.Texture

data GameState = GameState {
    playerPos    :: V2 CInt,
    playerDir    :: Direccion,
    isMoving     :: Bool,
    animFrame    :: Int,
    animTimer    :: Word32,
    assets       :: AssetManager,
    renderer     :: SDL.Renderer,
    shouldExit   :: Bool
}

type Game = StateT GameState IO

-- ==========================================
-- 3. CARGA DE ASSETS
-- ==========================================

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture r path = do
    texture <- SDL.Image.loadTexture r path
    putStrLn $ "Cargado: " ++ path
    return texture

cargarRecursos :: SDL.Renderer -> IO AssetManager
cargarRecursos r = do
    texHero  <- loadTexture r "Images/textures2D/Animations/hero_walk.png"
    texGrass <- loadTexture r "Images/textures2D/Tiles/grass.png"
    
    return $ M.fromList 
        [ ("hero", texHero)
        , ("grass", texGrass)
        ]

-- ==========================================
-- 4. LÓGICA (UPDATE)
-- ==========================================

handleEvents :: [SDL.EventPayload] -> Game ()
handleEvents events = do
    let quit = any isQuitEvent events
    when quit $ modify $ \s -> s { shouldExit = True }
    
    let (moveDir, moving) = foldl checkMovement (V2 0 0, False) events
    
    modify $ \s -> 
        let newPos = playerPos s + moveDir * 4 -- Velocidad
            currentDir = if moving then determineDir moveDir else playerDir s
        in s { 
            playerPos = newPos,
            playerDir = currentDir,
            isMoving = moving
        }

  where
    isQuitEvent SDL.QuitEvent = True
    isQuitEvent (SDL.KeyboardEvent k) = 
        SDL.keyboardEventKeyMotion k == SDL.Pressed &&
        SDL.keysymKeycode (SDL.keyboardEventKeysym k) == SDL.KeycodeQ
    isQuitEvent _ = False

    checkMovement (vec, _) (SDL.KeyboardEvent k) 
        | SDL.keyboardEventKeyMotion k == SDL.Pressed = 
            case SDL.keysymKeycode (SDL.keyboardEventKeysym k) of
                SDL.KeycodeUp    -> (V2 0 (-1), True)
                SDL.KeycodeDown  -> (V2 0 1, True)
                SDL.KeycodeLeft  -> (V2 (-1) 0, True)
                SDL.KeycodeRight -> (V2 1 0, True)
                _                -> (vec, False)
    checkMovement acc _ = acc

    determineDir (V2 0 (-1)) = Arriba
    determineDir (V2 0 1)    = Abajo
    determineDir (V2 (-1) 0) = Izquierda
    determineDir (V2 1 0)    = Derecha
    determineDir _           = Abajo

updateAnimation :: Word32 -> Game ()
updateAnimation ticks = do
    st <- get
    if isMoving st && (ticks - animTimer st > 150)
        then put $ st { 
            animFrame = (animFrame st + 1) `mod` 4,
            animTimer = ticks 
        }
        else unless (isMoving st) $ 
            put $ st { animFrame = 0 }

-- ==========================================
-- 5. RENDERIZADO
-- ==========================================

renderMapa :: SDL.Renderer -> AssetManager -> Game ()
renderMapa r texs = do
    case M.lookup "grass" texs of
        Nothing -> return ()
        Just texGrass -> do
            let srcSize = tileSizeSource -- Usa la constante de arriba (32)
            
            -- Toma el primer tile (0,0). 
            let srcRect = SDL.Rectangle (P (V2 0 0)) (V2 srcSize srcSize)

            forM_ (zip [0..] mapaPrueba) $ \(y, row) -> do
                forM_ (zip [0..] row) $ \(x, tileType) -> do
                    when (tileType == 0) $ do
                        let destX = x * screenSize
                        let destY = y * screenSize
                        let destRect = SDL.Rectangle (P (V2 destX destY)) (V2 screenSize screenSize)
                        SDL.copy r texGrass (Just srcRect) (Just destRect)

render :: Game ()
render = do
    st <- get
    let r = renderer st
    let texs = assets st
    
    SDL.rendererDrawColor r SDL.$= V4 0 0 0 255
    SDL.clear r

    renderMapa r texs

    case M.lookup "hero" texs of
        Just tex -> do
            let spriteS = heroSize -- Usa la constante de arriba (32)
            
            let fila = fromIntegral (fromEnum (playerDir st))
            let col  = fromIntegral (animFrame st)
            
            -- Recorte exacto de 32x32
            let srcRect = SDL.Rectangle 
                            (P (V2 (col * spriteS) (fila * spriteS))) 
                            (V2 spriteS spriteS)

            let (V2 px py) = playerPos st
            -- Dibujo en pantalla de 64x64
            let destRect = SDL.Rectangle (P (V2 px py)) (V2 screenSize screenSize)

            SDL.copy r tex (Just srcRect) (Just destRect)

        Nothing -> return ()

    SDL.present r

-- ==========================================
-- 6. MAIN
-- ==========================================

gameLoop :: Game ()
gameLoop = do
    ticks <- SDL.ticks
    events <- map SDL.eventPayload <$> SDL.pollEvents
    handleEvents events
    updateAnimation ticks
    render
    exit <- gets shouldExit
    unless exit $ do
        liftIO $ SDL.delay 16
        gameLoop

main :: IO ()
main = do
    SDL.initializeAll
    SDL.Image.initialize [SDL.Image.InitPNG]

    window <- SDL.createWindow "Haski RPG" SDL.defaultWindow {
        SDL.windowInitialSize = V2 800 600
    }
    r <- SDL.createRenderer window (-1) SDL.defaultRenderer

    textures <- cargarRecursos r

    let estadoInicial = GameState {
        playerPos = V2 100 100,
        playerDir = Abajo,
        isMoving = False,
        animFrame = 0,
        animTimer = 0,
        assets = textures,
        renderer = r,
        shouldExit = False
    }

    runStateT gameLoop estadoInicial

    SDL.destroyRenderer r
    SDL.destroyWindow window
    SDL.quit