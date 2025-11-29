{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font
import qualified SDL.Mixer as Mixer
import qualified Data.Map as M -- <--- IMPORTANTE: Agregado para usar el mapa de música
import Linear (V2(..))
import Control.Monad.State
import Control.Monad (unless)
import System.Random (randomRIO)
import Data.List (splitAt)
import Foreign.C.Types(CInt)

import Config
import Types
import Assets
import Logic
import Render
import Maps

gameLoop :: Game ()
gameLoop = do
    ticks <- SDL.ticks
    events <- map SDL.eventPayload <$> SDL.pollEvents

    handleEvents events ticks
    updateGame ticks
    render

    exit <- gets shouldExit
    unless exit $ do
        liftIO $ SDL.delay 16
        gameLoop

-- ==========================================
-- UTILIDADES DE INICIALIZACIÓN ALEATORIA
-- ==========================================

getRandomUniqueElements :: Int -> [a] -> IO [a]
getRandomUniqueElements n list = do
    let len = length list
    if n <= 0 || len == 0 || n > len
        then return []
        else do
            randomIndex <- randomRIO (0, len - 1)
            let selectedElement = list !! randomIndex

            let (before, after) = splitAt randomIndex list
            let remainingList = before ++ (tail after)

            rest <- getRandomUniqueElements (n - 1) remainingList
            return (selectedElement : rest)

-- Función para crear la lista inicial de ítems
createRandomItems :: [ItemType] -> [V2 CInt] -> IO [Item]
createRandomItems itemTypes floorPositions = do
    let totalItems = length itemTypes

    randomPositions <- getRandomUniqueElements totalItems floorPositions
    return $ zipWith (\tipo pos -> Item { itemType = tipo, itemPos = pos, itemObtained = False }) itemTypes randomPositions

main :: IO ()
main = do
    SDL.initializeAll
    SDL.Image.initialize [SDL.Image.InitPNG]
    SDL.Font.initialize

    -- Inicializar Audio MP3
    Mixer.initialize [Mixer.InitMP3] 
    Mixer.openAudio Mixer.defaultAudio 256

    window <- SDL.createWindow "Haski RPG" SDL.defaultWindow {
        SDL.windowInitialSize = V2 windowW windowH
    }
    r <- SDL.createRenderer window (-1) SDL.defaultRenderer

    misRecursos <- cargarRecursos r

    -- === CORRECCIÓN MÚSICA ===
    case M.lookup 1 (rMusic misRecursos) of
        -- CAMBIA ESTA LÍNEA (Pon el -1 antes):
        Just musicaNivel1 -> Mixer.playMusic (-1) musicaNivel1 
        Nothing -> putStrLn "Advertencia: No se encontró música para el Nivel 1"
    
    let startPos = V2 (54 * screenSize) (20 * screenSize)

    -- Creamos al jugador
    let jugador = createPlayer Hero startPos

    -- Usamos el mapa del Nivel 1 para generar items
    let allFloorPositions = getFloorPositions mapaNivel1

    let itemsToSpawn = [ PotionFuerza, PotionFuerza
                        , PotionVelocidad, PotionVelocidad
                        , PotionVeneno, PotionVeneno
                        , PotionInvisibilidad, PotionInvisibilidad
                        ]

    randomItems <- createRandomItems itemsToSpawn allFloorPositions

    -- DEFINICIÓN DEL ESTADO INICIAL
    -- Nota: Eliminé las variables 'orco', 'zombie', 'vaca' sueltas
    -- porque ahora usas 'generarEnemigos 1' directamente.
    
    let estadoInicial = GameState {
        player      = jugador,
        
        -- Generamos enemigos del Nivel 1 (Ratas)
        enemies     = generarEnemigos 1, 
        
        mapItems    = randomItems,
        gameLog     = ["Bienvenido a la mazmorra."],
        resources   = misRecursos,
        renderer    = r,
        shouldExit  = False,
        gameMode    = TitleScreen,
        menuSelection = 0,
        gameStartTime = 0,
        gameOverTimer = 0, 
        encounteredTypes = [],
        
        -- Sistema de Niveles
        currentLevel = 1,
        currentMap = mapaNivel1 -- Mapa con escalera
    }

    runStateT gameLoop estadoInicial

    -- === LIMPIEZA FINAL ===
    -- Liberamos todas las canciones del mapa
    
    Mixer.closeAudio
    SDL.destroyRenderer r
    SDL.destroyWindow window
    SDL.Font.quit
    Mixer.quit
    SDL.quit