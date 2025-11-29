{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font
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
    SDL.Font.initialize

    window <- SDL.createWindow "Haski RPG" SDL.defaultWindow {
        SDL.windowInitialSize = V2 windowW windowH
    }
    r <- SDL.createRenderer window (-1) SDL.defaultRenderer

    misRecursos <- cargarRecursos r

    let startPos = V2 (54 * screenSize) (20 * screenSize)

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

    let zombiePos = V2 (15 * screenSize) (10 * screenSize)
    let zombie = Entity {
        entPos = zombiePos, entTarget = zombiePos, entOrigin = zombiePos,
        entDir = Abajo, entIsMoving = False, entAnimFrame = 0, entAnimTimer = 0,
        entSpeed = 5,

        entClass = Zombie,
        entHp = 20,
        entMaxHp = 20,
        entMinAtk = 1,
        entMaxAtk = 2,

        entXp = 30, entLevel = 1, entNextLevel = 0,
        entCooldown = 0, entAggro = False,
        entPatrolTimer = 0,
        entDead = False, entDeathTick = 0, entRegenTick = 0
    }

    -- NUEVO ENEMIGO: VACA
    let cowPos = V2 (45 * screenSize) (30 * screenSize)
    let vaca = Entity {
        entPos = cowPos, entTarget = cowPos, entOrigin = cowPos,
        entDir = Abajo, entIsMoving = False, entAnimFrame = 0, entAnimTimer = 0,
        entSpeed = 10, -- Casi tan rápida como tú (12)

        entClass = Vaca,
        entHp = 60, -- Mucha vida (Tanque)
        entMaxHp = 60,
        entMinAtk = 5, -- Golpea fuerte
        entMaxAtk = 8,

        entXp = 100, entLevel = 1, entNextLevel = 0,
        entCooldown = 0, entAggro = False,
        entPatrolTimer = 0,
        entDead = False, entDeathTick = 0, entRegenTick = 0
    }

    let estadoInicial = GameState {
        player      = jugador,
        enemies     = [orco, zombie, vaca], -- ¡Vaca añadida!
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
    SDL.Font.quit
    SDL.quit
