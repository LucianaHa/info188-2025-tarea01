module Assets where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font -- Necesario para la fuente
import Data.Map (fromList)
import Control.Exception (try, SomeException) -- Para carga segura de fuente
import Types (Resources(..)) -- Devuelve la nueva estructura Resources

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture r path = SDL.Image.loadTexture r path

-- CÓDIGO COLABORADOR: Carga de Fuente Segura
loadFontSafe :: FilePath -> Int -> IO (Maybe SDL.Font.Font)
loadFontSafe path size = do
    result <- try (SDL.Font.load path size) :: IO (Either SomeException SDL.Font.Font)
    case result of
        Left _ -> do
            putStrLn $ "ADVERTENCIA: No se pudo cargar la fuente: " ++ path
            return Nothing
        Right font -> return (Just font)

cargarRecursos :: SDL.Renderer -> IO Resources -- Devuelve Resources
cargarRecursos r = do
    -- 1. Cargar Tileset (MAPA)
    texDungeon <- loadTexture r "Images/textures2D/Tiles/dungeon.png"
    
    -- 2. Cargar Fondo del Menú
    texBackground <- loadTexture r "Images/HUD/TitleScreen/title.jpg" 

    -- 3. Cargar el Sprite del Héroe
    texHeroe <- loadTexture r "Images/textures2D/Animations/hero.png" 

    -- 4. CARGAR EL OGRO
    texOgre <- loadTexture r "Images/textures2D/Animations/ogre.png"

    -- 5. Cargar Fuente
    miFuente <- loadFontSafe "assets/PressStart2P.ttf" 12 
    
    -- Retornar la estructura Resources completa
    return $ Resources
        { rTextures = fromList
            [ ("dungeon", texDungeon) 
            , ("background", texBackground)
            , ("hero", texHeroe)
            , ("ogre", texOgre) 
            ]
        , rFont     = miFuente
        }