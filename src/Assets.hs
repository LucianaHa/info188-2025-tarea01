module Assets where

import qualified SDL
import qualified SDL.Image
import Data.Map (fromList)
import Types (AssetManager)

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture r path = SDL.Image.loadTexture r path

cargarRecursos :: SDL.Renderer -> IO AssetManager
cargarRecursos r = do
    -- 1. Cargar Tileset
    texDungeon <- loadTexture r "Images/textures2D/Tiles/dungeon.png"
    
    -- 2. Cargar Fondo del Menú: USANDO LA RUTA Y EXTENSIÓN CORRECTAS
    texBackground <- loadTexture r "Images/HUD/TitleScreen/title.jpg" 
    
    return $ fromList 
        [ ("dungeon", texDungeon)
        , ("background", texBackground) -- Guardamos con clave "background"
        ]