module Assets where

import qualified SDL
import qualified SDL.Image
import Data.Map (fromList)
import Types (AssetManager)

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture r path = SDL.Image.loadTexture r path

cargarRecursos :: SDL.Renderer -> IO AssetManager
cargarRecursos r = do
    -- SOLO CARGAMOS EL ARCHIVO PRINCIPAL
    texDungeon <- loadTexture r "Images/textures2D/Tiles/dungeon.png"
    
    return $ fromList 
        [ ("dungeon", texDungeon) -- Esta textura servirá para mapa Y héroe
        ]