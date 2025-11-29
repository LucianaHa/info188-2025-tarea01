module Types where

import qualified SDL
import qualified SDL.Font
import Linear (V2)
import Foreign.C.Types (CInt)
import Data.Map (Map)
import Data.Word (Word32)
import Control.Monad.State (StateT)

data GameMode = TitleScreen | Playing
    deriving (Show, Eq)

data Direccion = Abajo | Izquierda | Derecha | Arriba
    deriving (Show, Eq)

-- AÑADIDO: Zombie a las clases
data Clase = Guerrero | Mago | Asesino | Orco | Esqueleto | Zombie
    deriving (Show, Eq)

type AssetManager = Map String SDL.Texture

data Resources = Resources
    { rTextures :: AssetManager
    , rFont     :: Maybe SDL.Font.Font
    }

data Entity = Entity {
    entPos       :: V2 CInt,
    entTarget    :: V2 CInt,
    entOrigin    :: V2 CInt,
    entDir       :: Direccion,
    entIsMoving  :: Bool,
    entAnimFrame :: Int,
    entAnimTimer :: Word32,
    entSpeed     :: CInt,

    entClass     :: Clase,
    entHp        :: Int,
    entMaxHp     :: Int,
    entMinAtk    :: Int,
    entMaxAtk    :: Int,

    entXp        :: Int,
    entLevel     :: Int,
    entNextLevel :: Int,

    entCooldown  :: Word32,
    entAggro     :: Bool,
    entPatrolTimer :: Word32,

    entDead      :: Bool,
    entDeathTick :: Word32,
    entRegenTick :: Word32
} deriving (Show, Eq)

data GameState = GameState {
    player      :: Entity,
    enemies     :: [Entity],
    gameLog     :: [String],
    resources   :: Resources,
    renderer    :: SDL.Renderer,
    shouldExit  :: Bool,
    gameMode    :: GameMode,
    menuSelection :: Int
}

type Game = StateT GameState IO
