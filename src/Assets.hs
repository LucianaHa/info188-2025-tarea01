module Assets where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font
import Data.Map (fromList)
import Control.Exception (try, SomeException)
import Types (Resources(..))

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture r path = SDL.Image.loadTexture r path

loadFontSafe :: FilePath -> Int -> IO (Maybe SDL.Font.Font)
loadFontSafe path size = do
    result <- try (SDL.Font.load path size) :: IO (Either SomeException SDL.Font.Font)
    case result of
        Left _ -> do
            putStrLn $ "ADVERTENCIA: No se pudo cargar la fuente: " ++ path
            return Nothing
        Right font -> return (Just font)

cargarRecursos :: SDL.Renderer -> IO Resources
cargarRecursos r = do
    -- 1. Cargar Tileset
    texDungeon <- loadTexture r "Images/textures2D/Tiles/dungeon.png"

    -- 2. Cargar Fondo del Menú (Del cambio entrante)
    -- Asegúrate de tener esta imagen o el juego podría fallar al intentar dibujarla si la llamas
    texBackground <- loadTexture r "Images/HUD/TitleScreen/title.jpg"

    -- 3. Cargar Fuente (De tu código HEAD)
    -- ¡Asegúrate de que el archivo se llame 'PressStart2P.ttf' en la carpeta 'assets'!
    miFuente <- loadFontSafe "assets/PressStart2P.ttf" 12

    -- 4. Retornar la estructura Resources completa
    return $ Resources
        { rTextures = fromList
            [ ("dungeon", texDungeon)
            , ("background", texBackground) -- Agregamos el fondo al mapa
            ]
        , rFont     = miFuente
        }
