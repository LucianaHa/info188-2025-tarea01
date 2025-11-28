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
    texDungeon <- loadTexture r "Images/textures2D/Tiles/dungeon.png"

    -- CAMBIO: Usamos tamaño 12 y buscamos la fuente específica
    -- ¡Asegúrate de que el archivo se llame 'PressStart2P.ttf' en la carpeta 'assets'!
    miFuente <- loadFontSafe "assets/PressStart2P.ttf" 12

    return $ Resources
        { rTextures = fromList [ ("dungeon", texDungeon) ]
        , rFont     = miFuente
        }
