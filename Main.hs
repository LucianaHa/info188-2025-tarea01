{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font
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

    window <- SDL.createWindow "Haski RPG" SDL.defaultWindow {
        SDL.windowInitialSize = V2 windowW windowH
    }
    r <- SDL.createRenderer window (-1) SDL.defaultRenderer

    misRecursos <- cargarRecursos r

    let startPos = V2 (54 * screenSize) (20 * screenSize)

    let jugador = createPlayer Hero startPos

    let allFloorPositions = getFloorPositions mapaSuelo

    let itemsToSpawn = [ PotionFuerza, PotionFuerza
                        , PotionVelocidad, PotionVelocidad
                        , PotionVeneno, PotionVeneno
                        , PotionInvisibilidad, PotionInvisibilidad
                        ]

    randomItems <- createRandomItems itemsToSpawn allFloorPositions

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
        entBuffAtkEnd = 0,
        entBuffSpdEnd = 0,
        entInvisible  = False,
        entInvEnd     = 0,

        entBaseMinAtk = 2,
        entBaseMaxAtk = 4,
        entBaseSpeed  = 3,
        entDead = False, entDeathTick = 0, entRegenTick = 0,

        entAttackType = NoAttack,
        entAttackTimer = 0
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

        entBuffAtkEnd = 0,
        entBuffSpdEnd = 0,
        entInvisible  = False,
        entInvEnd     = 0,

        entBaseMinAtk = 1,
        entBaseMaxAtk = 2,
        entBaseSpeed  = 5,
        entDead = False, entDeathTick = 0, entRegenTick = 0,

        entAttackType = NoAttack,
        entAttackTimer = 0
    }

    let cowPos = V2 (45 * screenSize) (30 * screenSize)
    let vaca = Entity {
        entPos = cowPos, entTarget = cowPos, entOrigin = cowPos,
        entDir = Abajo, entIsMoving = False, entAnimFrame = 0, entAnimTimer = 0,
        entSpeed = 10,

        entClass = Vaca,
        entHp = 60,
        entMaxHp = 60,
        entMinAtk = 5,
        entMaxAtk = 8,

        entXp = 100, entLevel = 1, entNextLevel = 0,
        entCooldown = 0, entAggro = False,
        entPatrolTimer = 0,

        entBuffAtkEnd = 0,
        entBuffSpdEnd = 0,
        entInvisible  = False,
        entInvEnd     = 0,

        entBaseMinAtk = 5,
        entBaseMaxAtk = 8,
        entBaseSpeed  = 10,

        entDead = False, entDeathTick = 0, entRegenTick = 0,

        entAttackType = NoAttack,
        entAttackTimer = 0
    }
    let estadoInicial = GameState {
        player      = jugador,
        enemies     = [orco, zombie, vaca],
        mapItems    = randomItems,
        gameLog     = ["Bienvenido a la mazmorra."],
        resources   = misRecursos,
        renderer    = r,
        shouldExit  = False,
        gameMode    = TitleScreen,
        menuSelection = 0,
        gameStartTime = 0,
        gameOverTimer = 0, -- Inicializado en 0
        encounteredTypes = []
    }

    runStateT gameLoop estadoInicial

    SDL.destroyRenderer r
    SDL.destroyWindow window
    SDL.Font.quit
    SDL.quit
