{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font -- Importar Font
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
    SDL.Font.initialize -- Inicializar SDL_ttf

    window <- SDL.createWindow "Haski RPG" SDL.defaultWindow {
        SDL.windowInitialSize = V2 windowW windowH
    }
    r <- SDL.createRenderer window (-1) SDL.defaultRenderer

    misRecursos <- cargarRecursos r

    let startPos = V2 (54 * screenSize) (20 * screenSize)
    
    -- JUGADOR INICIAL
    let jugador = Entity {
        entPos = startPos, entTarget = startPos, entOrigin = startPos,
        entDir = Izquierda, entIsMoving = False, entAnimFrame = 0, entAnimTimer = 0,
        entSpeed = 12,

        entClass = Guerrero,
        entHp = 20, entMaxHp = 20,
        entMinAtk = 6, entMaxAtk = 8,

        entXp = 0, entLevel = 1, entNextLevel = 100,
        entCooldown = 0, entAggro = False,
        entPatrolTimer = 0, 
        entDead = False, entDeathTick = 0, entRegenTick = 0
    }

    -- ENEMIGO (ORCO)
    let orcoPos = V2 (10 * screenSize) (45 * screenSize)
    let orco = Entity {
        entPos = orcoPos, entTarget = orcoPos, entOrigin = orcoPos,
        entDir = Abajo, entIsMoving = False, entAnimFrame = 0, entAnimTimer = 0,
        entSpeed = 3,

        entClass = Orco,
        entHp = 30, entMaxHp = 30,
        entMinAtk = 2, entMaxAtk = 4,

        entXp = 50, entLevel = 1, entNextLevel = 0,
        entCooldown = 0, entAggro = False,
        entPatrolTimer = 0, 
        entDead = False, entDeathTick = 0, entRegenTick = 0
    }

    -- ESTADO INICIAL COMPLETO
    let estadoInicial = GameState {
        player      = jugador,
        enemies     = [orco],
        gameLog     = ["Bienvenido a la mazmorra."],
        resources   = misRecursos, 
        renderer    = r,
        shouldExit  = False,
        gameMode    = TitleScreen,
        menuSelection = 0
    }
    
    runStateT gameLoop estadoInicial

    SDL.destroyRenderer r
    SDL.destroyWindow window
    SDL.Font.quit -- Limpieza de SDL_ttf
    SDL.quit