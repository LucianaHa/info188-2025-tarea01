{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Image
import Linear (V2(..))
import Control.Monad.State
import Control.Monad (unless)

import Config
import Types
import Assets
import Logic
import Render

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

    window <- SDL.createWindow "Haski RPG" SDL.defaultWindow {
        SDL.windowInitialSize = V2 windowW windowH
    }
    r <- SDL.createRenderer window (-1) SDL.defaultRenderer

    textures <- cargarRecursos r
    
    -- POSICIÓN SEGURA (En la Sala D, a la derecha del mapa)
    -- Tile 54, 20 es suelo seguro según el mapa que creamos.
    let startPos = V2 (54 * screenSize) (20 * screenSize)
    
    let jugador = Entity {
        entPos = startPos, entTarget = startPos, entDir = Izquierda,
        entIsMoving = False, entAnimFrame = 0, entAnimTimer = 0,
        entClass = Guerrero,
        entHp = 20, entMaxHp = 20,
        entMinAtk = 6, entMaxAtk = 8,
        entCooldown = 0, entAggro = False
    }

    -- Un enemigo de prueba en la Sala A (Izquierda Abajo)
    let orco = Entity {
        entPos = V2 (10 * screenSize) (45 * screenSize), 
        entTarget = V2 (10 * screenSize) (45 * screenSize), 
        entDir = Abajo,
        entIsMoving = False, entAnimFrame = 0, entAnimTimer = 0,
        entClass = Orco,
        entHp = 30, entMaxHp = 30,
        entMinAtk = 2, entMaxAtk = 4,
        entCooldown = 0, entAggro = False
    }

    let estadoInicial = GameState {
        player      = jugador,
        enemies     = [orco],
        assets      = textures,
        renderer    = r,
        shouldExit  = False
    }

    runStateT gameLoop estadoInicial

    SDL.destroyRenderer r
    SDL.destroyWindow window
    SDL.quit