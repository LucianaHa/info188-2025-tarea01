module Types where

import qualified SDL
import Linear (V2)
import Foreign.C.Types (CInt)
import Data.Map (Map)
import Data.Word (Word32)
import Control.Monad.State (StateT)

-- DIRECCIONES
data Direccion = Abajo | Izquierda | Derecha | Arriba 
    deriving (Show, Eq)

-- CLASES (Visuales)
data Clase = Guerrero | Mago | Asesino | Orco | Esqueleto -- Añadimos enemigos
    deriving (Show, Eq)

-- ASSETS
type AssetManager = Map String SDL.Texture

-- NUEVO: ENTIDAD (Sirve para Jugador y Enemigos)
data Entity = Entity {
    -- Físicas
    entPos       :: V2 CInt,
    entTarget    :: V2 CInt,
    entDir       :: Direccion,
    entIsMoving  :: Bool,
    entAnimFrame :: Int,
    entAnimTimer :: Word32,
    
    -- Estadísticas RPG
    entClass     :: Clase,
    entHp        :: Int,
    entMaxHp     :: Int,
    entMinAtk    :: Int, -- Daño Mínimo (ej: 6)
    entMaxAtk    :: Int, -- Daño Máximo (ej: 8)
    
    -- Estado de Combate
    entCooldown  :: Word32, -- Tiempo para volver a atacar
    entAggro     :: Bool    -- ¿Está enojado? (Para enemigos)
} deriving (Show, Eq)

-- ESTADO DEL JUEGO (Refactorizado)
data GameState = GameState {
    player      :: Entity,    -- El Jugador es una Entidad
    enemies     :: [Entity],  -- Lista de enemigos
    
    assets      :: AssetManager,
    renderer    :: SDL.Renderer,
    shouldExit  :: Bool
}

type Game = StateT GameState IO