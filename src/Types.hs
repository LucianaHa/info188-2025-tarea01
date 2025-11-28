module Types where

import qualified SDL
import Linear (V2)
import Foreign.C.Types (CInt)
import Data.Map (Map)
import Data.Word (Word32)
import Control.Monad.State (StateT)

data GameMode = TitleScreen | Playing -- Tu código
    deriving (Show, Eq)

data Direccion = Abajo | Izquierda | Derecha | Arriba
    deriving (Show, Eq) -- Código final

data Clase = Guerrero | Mago | Asesino | Orco | Esqueleto
    deriving (Show, Eq)

type AssetManager = Map String SDL.Texture

data Entity = Entity {
    -- Físicas
    entPos       :: V2 CInt,
    entTarget    :: V2 CInt,
    entOrigin    :: V2 CInt, -- Combinado
    entDir       :: Direccion,
    entIsMoving  :: Bool,
    entAnimFrame :: Int,
    entAnimTimer :: Word32,
    entSpeed     :: CInt,     -- Código colaborador

    -- Estadísticas
    entClass     :: Clase,
    entHp        :: Int,
    entMaxHp     :: Int,
    entMinAtk    :: Int,
    entMaxAtk    :: Int,

    -- Progresión (XP)
    entXp        :: Int, -- Combinado
    entLevel     :: Int, -- Combinado
    entNextLevel :: Int, -- Combinado

    -- Estado
    entCooldown  :: Word32,
    entAggro     :: Bool,
    entPatrolTimer :: Word32, -- Código colaborador

    -- Muerte y Regen
    entDead      :: Bool,   -- Combinado
    entDeathTick :: Word32, -- Combinado
    entRegenTick :: Word32  -- Combinado
} deriving (Show, Eq)

data GameState = GameState {
    player      :: Entity,
    enemies     :: [Entity],
    
    gameLog     :: [String],
    assets      :: AssetManager,
    renderer    :: SDL.Renderer,
    shouldExit  :: Bool,
    
    -- Modo actual del juego (Tu código)
    gameMode    :: GameMode,
    
    -- AÑADIDO: Opción seleccionada en el menú (Tu código)
    menuSelection :: Int 
}

type Game = StateT GameState IO