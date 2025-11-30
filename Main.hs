{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font
import qualified SDL.Mixer as Mixer
import qualified Data.Map as M 
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

main :: IO ()
main = do
    SDL.initializeAll
    SDL.Image.initialize [SDL.Image.InitPNG]
    SDL.Font.initialize

    -- CONFIGURACIÓN DE AUDIO (Del equipo, es mejor)
    let audioConfig = Mixer.Audio 48000 Mixer.FormatS16_Sys Mixer.Stereo
    Mixer.openAudio audioConfig 4096
    
    _ <- Mixer.setChannels 32

    window <- SDL.createWindow "Haski RPG" SDL.defaultWindow {
        SDL.windowInitialSize = V2 windowW windowH
    }
    r <- SDL.createRenderer window (-1) SDL.defaultRenderer

    misRecursos <- cargarRecursos r

    -- MÚSICA INICIAL (Tu lógica de niveles)
    case M.lookup 1 (rMusic misRecursos) of
        Just musicaNivel1 -> Mixer.playMusic (-1) musicaNivel1 
        Nothing -> putStrLn "Advertencia: No se encontró música para el Nivel 1"

    let startPos = V2 (54 * screenSize) (20 * screenSize)
    let jugador = createPlayer Hero startPos
    
    let allFloorPositions = getFloorPositions mapaNivel1
    let itemsToSpawn = [ PotionFuerza, PotionFuerza
                        , PotionVelocidad, PotionVelocidad
                        , PotionVeneno, PotionVeneno
                        , PotionInvisibilidad, PotionInvisibilidad ]

    randomItems <- createRandomItems itemsToSpawn allFloorPositions

    let estadoInicial = GameState {
        player      = jugador,
        enemies     = generarEnemigos 1, -- Usamos tu generador
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
        currentLevel = 1,
        currentMap = mapaNivel1
    }

    runStateT gameLoop estadoInicial

    Mixer.closeAudio
    Mixer.quit
    SDL.destroyRenderer r
    SDL.destroyWindow window
    SDL.Font.quit
    SDL.quit