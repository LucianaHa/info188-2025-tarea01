module Types where

import qualified SDL
import Linear (V2)
import Foreign.C.Types (CInt)
import Data.Map (Map)
import Data.Word (Word32)
import Control.Monad.State (StateT)

data Direccion = Abajo | Izquierda | Derecha | Arriba 
    deriving (Show, Eq)

data Clase = Guerrero | Mago | Asesino | Orco | Esqueleto
    deriving (Show, Eq)

type AssetManager = Map String SDL.Texture

data Entity = Entity {
    -- Físicas
    entPos       :: V2 CInt,
    entTarget    :: V2 CInt,
    entOrigin    :: V2 CInt, -- NUEVO: Dónde nació (para Respawn)
    entDir       :: Direccion,
    entIsMoving  :: Bool,
    entAnimFrame :: Int,
    entAnimTimer :: Word32,
    
    -- Estadísticas
    entClass     :: Clase,
    entHp        :: Int,
    entMaxHp     :: Int,
    entMinAtk    :: Int,
    entMaxAtk    :: Int,
    
    -- Progresión (XP)
    entXp        :: Int, -- XP Actual (Jugador) o XP que da (Enemigo)
    entLevel     :: Int,
    entNextLevel :: Int, -- XP para el siguiente nivel
    
    -- Estado
    entCooldown  :: Word32,
    entAggro     :: Bool,
    
    -- Muerte y Regen
    entDead      :: Bool,   -- ¿Está muerto?
    entDeathTick :: Word32, -- ¿Cuándo murió?
    entRegenTick :: Word32  -- Última vez que regeneró vida
} deriving (Show, Eq)

data GameState = GameState {
    player      :: Entity,
    enemies     :: [Entity],
    
    -- NUEVO: Log de mensajes
    gameLog     :: [String],
    
    assets      :: AssetManager,
    renderer    :: SDL.Renderer,
    shouldExit  :: Bool
}

type Game = StateT GameState IO