module Types where

import qualified SDL
import qualified SDL.Font -- <--- Importamos Font
import Linear (V2)
import Foreign.C.Types (CInt)
import Data.Map (Map)
import Data.Word (Word32)
import Control.Monad.State (StateT)

-- | Modos del juego: Pantalla de título o Jugando
data GameMode = TitleScreen | Playing
    deriving (Show, Eq)

data Direccion = Abajo | Izquierda | Derecha | Arriba
    deriving (Show, Eq)

data Clase = Guerrero | Mago | Asesino | Orco | Esqueleto
    deriving (Show, Eq)

-- | Gestor de recursos: Texturas y Fuentes
data Resources = Resources {
    rTextures :: Map String SDL.Texture,
    rFont     :: Maybe SDL.Font.Font
}

type AssetManager = Map String SDL.Texture -- Mantenemos alias por compatibilidad si se usa en otros lados

-- | Entidad del juego (Jugador o Enemigo)
data Entity = Entity {
    -- Físicas
    entPos       :: V2 CInt,
    entTarget    :: V2 CInt,
    entOrigin    :: V2 CInt,
    entDir       :: Direccion,
    entIsMoving  :: Bool,
    entAnimFrame :: Int,
    entAnimTimer :: Word32,
    entSpeed     :: CInt,    -- Velocidad de movimiento

    -- Estadísticas
    entClass     :: Clase,
    entHp        :: Int,
    entMaxHp     :: Int,
    entMinAtk    :: Int,
    entMaxAtk    :: Int,

    -- Progresión (XP)
    entXp        :: Int,
    entLevel     :: Int,
    entNextLevel :: Int,

    -- Estado e IA
    entCooldown    :: Word32,
    entAggro       :: Bool,
    entPatrolTimer :: Word32, -- Temporizador para patrulla aleatoria

    -- Muerte y Regen
    entDead      :: Bool,
    entDeathTick :: Word32,
    entRegenTick :: Word32
} deriving (Show, Eq)

-- | Estado global del juego
data GameState = GameState {
    player        :: Entity,
    enemies       :: [Entity],

    gameLog       :: [String],
    resources     :: Resources, -- Recursos cargados (imágenes, fuentes)
    renderer      :: SDL.Renderer,
    shouldExit    :: Bool,

    -- Control de Flujo
    gameMode      :: GameMode, -- ¿Estamos en menú o jugando?
    menuSelection :: Int       -- Opción seleccionada en el menú
}

type Game = StateT GameState IO
