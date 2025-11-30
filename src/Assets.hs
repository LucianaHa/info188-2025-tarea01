module Assets where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font
import qualified SDL.Mixer as Mixer
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

loadMusicSafe :: FilePath -> IO (Maybe Mixer.Music)
loadMusicSafe path = do
    result <- try (Mixer.load path) :: IO (Either SomeException Mixer.Music)
    case result of
        Left err -> do
            putStrLn $ "ADVERTENCIA MUSICA: " ++ show err 
            return Nothing
        Right music -> return (Just music)

loadChunkSafe :: FilePath -> IO (Maybe Mixer.Chunk)
loadChunkSafe path = do
    result <- try (Mixer.load path) :: IO (Either SomeException Mixer.Chunk)
    case result of
        Left _ -> do
            putStrLn $ "ADVERTENCIA SFX: No se encontró: " ++ path
            return Nothing
        Right chunk -> return (Just chunk)

cargarRecursos :: SDL.Renderer -> IO Resources
cargarRecursos r = do
    -- TEXTURAS
    texDungeon <- loadTexture r "Images/textures2D/Tiles/dungeon.png"
    texBackground <- loadTexture r "Images/HUD/TitleScreen/title.jpg"
    texShield <- loadTexture r "Images/textures2D/Animations/shield.png"
    texGameOver <- loadTexture r "Images/HUD/GameOver/background.png"
    texBlades <- loadTexture r "Images/textures2D/Animations/bladesHit.png"
    texItems <- loadTexture r "Images/HUD/Icons/Potions.png"
    
    texHeroe <- loadTexture r "Images/textures2D/Animations/hero.png"
    texOgre <- loadTexture r "Images/textures2D/Animations/ogre.png"
    texZombie <- loadTexture r "Images/textures2D/Animations/zombie.png"
    texCow <- loadTexture r "Images/textures2D/Animations/cow-white.png"
    texRat <- loadTexture r "Images/textures2D/Animations/rat.png"
    
    texPaladin <- loadTexture r "Images/textures2D/Animations/paladin.png"
    texBruja   <- loadTexture r "Images/textures2D/Animations/bruja.png"
    texChamana <- loadTexture r "Images/textures2D/Animations/chamana.png"

    -- FUENTE
    miFuente <- loadFontSafe "assets/PressStart2P.ttf" 12

    -- MÚSICA
    musicLvl1Maybe <- loadMusicSafe "assets/music.mp3"
    musicLvl2Maybe <- loadMusicSafe "assets/boss.mp3"

    let listaMusica = case (musicLvl1Maybe, musicLvl2Maybe) of
            (Just m1, Just m2) -> [(1, m1), (2, m2)]
            (Just m1, Nothing) -> [(1, m1)]
            _ -> []

    -- SFX
    sfxPasos <- loadChunkSafe "Music/human_walk_stone.ogg"
    sfxDano <- loadChunkSafe "Music/human_damage.ogg"
    sfxMuerte <- loadChunkSafe "Music/human_death_spin.ogg"
    sfxAtaque <- loadChunkSafe "Music/human_atk_sword.ogg"
    sfxFallo  <- loadChunkSafe "Music/sword_miss.ogg"
    sfxPocion <- loadChunkSafe "Music/human_charge.ogg"
    sfxVaca    <- loadChunkSafe "Music/Cow.ogg"

    return $ Resources
        { rTextures = fromList
            [ ("dungeon", texDungeon)
            , ("background", texBackground)
            , ("gameover", texGameOver)
            , ("items", texItems)
            , ("shield", texShield)
            , ("fx_blade", texBlades)
            , ("hero", texHeroe)
            , ("ogre", texOgre)
            , ("zombie", texZombie)
            , ("cow", texCow)
            , ("rat", texRat)
            , ("paladin", texPaladin)
            , ("bruja", texBruja)
            , ("chamana", texChamana)
            ]
        , rFont     = miFuente
        
        -- ESTRUCTURA FINAL LIMPIA
        , rMusic      = fromList listaMusica
        , rSfxStep    = sfxPasos
        , rSfxDamage  = sfxDano
        , rSfxDeath   = sfxMuerte
        , rSfxAttack  = sfxAtaque
        , rSfxMiss    = sfxFallo
        , rSfxPotion  = sfxPocion
        , rSfxCow     = sfxVaca
        }