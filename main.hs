{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Image
import Linear (V2(..))
import Control.Monad.State
import Control.Monad (unless)

-- Importamos nuestros nuevos módulos
import Config
import Types
import Assets
import Logic
import Render

gameLoop :: Game ()
gameLoop = do
    ticks <- SDL.ticks
    events <- map SDL.eventPayload <$> SDL.pollEvents
    
    handleEvents events
    updateMovement
    updateAnimation ticks
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
    
    let startPos = V2 (5 * screenSize) (5 * screenSize)

    let estadoInicial = GameState {
        pixelPos     = startPos,
        targetPos    = startPos,
        playerDir    = Abajo,
        isMoving     = False,
        animFrame    = 0,
        animTimer    = 0,
        assets       = textures,
        renderer     = r,
        shouldExit   = False
    }

    runStateT gameLoop estadoInicial

    SDL.destroyRenderer r
    SDL.destroyWindow window
    SDL.quit