module Types where

import qualified SDL
import qualified SDL.Font -- Necesario para Resources
import Linear (V2)
import Foreign.C.Types (CInt)
import Data.Map (Map)
import Data.Word (Word32)
import Control.Monad.State (StateT)

data GameMode = TitleScreen | Playing 
    deriving (Show, Eq)

data Direccion = Abajo | Izquierda | Derecha | Arriba
    deriving (Show, Eq) 

data Clase = Guerrero | Mago | Asesino | Orco | Esqueleto
    deriving (Show, Eq)

type AssetManager = Map String SDL.Texture

-- NUEVA ESTRUCTURA DE RECURSOS (CÓDIGO COLABORADOR)
data Resources = Resources
    { rTextures :: AssetManager
    , rFont     :: Maybe SDL.Font.Font
    }

data Entity = Entity {
    -- Físicas
    entPos       :: V2 CInt,
    entTarget    :: V2 CInt,
    entOrigin    :: V2 CInt, 
    entDir       :: Direccion,
    entIsMoving  :: Bool,
    entAnimFrame :: Int,
    entAnimTimer :: Word32,
    entSpeed     :: CInt,     

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

    -- Estado
    entCooldown  :: Word32,
    entAggro     :: Bool,
    entPatrolTimer :: Word32, 

    -- Muerte y Regen
    entDead      :: Bool,   
    entDeathTick :: Word32, 
    entRegenTick :: Word32  
} deriving (Show, Eq)

data GameState = GameState {
    player      :: Entity,
    enemies     :: [Entity],
    
    gameLog     :: [String],
    resources   :: Resources, -- CAMBIO CLAVE: Usamos Resources
    renderer    :: SDL.Renderer,
    shouldExit  :: Bool,
    
    -- Modo actual del juego
    gameMode    :: GameMode,
    
    -- AÑADIDO: Opción seleccionada en el menú 
    menuSelection :: Int 
}

type Game = StateT GameState IO