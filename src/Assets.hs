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
    texBackground <- loadTexture r "Images/HUD/TitleScreen/title.jpg"

    -- NUEVO: Game Over
    texGameOver <- loadTexture r "Images/HUD/GameOver/background.png"

    texHeroe <- loadTexture r "Images/textures2D/Animations/hero.png"
    texOgre <- loadTexture r "Images/textures2D/Animations/ogre.png"
    texZombie <- loadTexture r "Images/textures2D/Animations/zombie.png"
    texCow <- loadTexture r "Images/textures2D/Animations/cow-white.png"
    texItems <- loadTexture r "Images/HUD/Icons/Potions.png"
    miFuente <- loadFontSafe "assets/PressStart2P.ttf" 12

    return $ Resources
        { rTextures = fromList
            [ ("dungeon", texDungeon)
            , ("background", texBackground)
            , ("gameover", texGameOver) -- Añadido
            , ("hero", texHeroe)
            , ("ogre", texOgre)
            , ("zombie", texZombie)
            , ("cow", texCow)
            , ("items", texItems)
            ]
        , rFont     = miFuente
        }
