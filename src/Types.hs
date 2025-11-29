module Types (
    Direccion(..),
    Clase(..),
    ItemType(..),
    Item(..),
    AssetManager,
    Resources(..),
    Entity(..),
    GameState(..),
    Game,
    GameMode(..),
    AttackType(..)
) where

import qualified SDL
import qualified SDL.Font
import Linear (V2)
import Foreign.C.Types (CInt)
import Data.Map (Map)
import Data.Word (Word32)
import Control.Monad.State (StateT)
import qualified SDL.Mixer

-- AÑADIDO: GameOver
data GameMode = TitleScreen | Playing | GameOver
    deriving (Show, Eq)

data Direccion = Abajo | Izquierda | Derecha | Arriba
    deriving (Show, Eq)

data Clase = Hero | Paladin | Bruja | Chamana | Orco | Esqueleto | Zombie | Vaca
    deriving (Show, Eq)

data AttackType = NoAttack | AtkNormal | AtkArea
    deriving (Show, Eq)

data ItemType = PotionFuerza | PotionInvisibilidad | PotionVelocidad | PotionVeneno
		deriving (Show, Eq)

data Item = Item {
    itemType :: ItemType,
    itemPos  :: V2 CInt, -- Posición en el mapa
    itemObtained :: Bool -- Si ya fue recogido
} deriving (Show, Eq)

type AssetManager = Map String SDL.Texture

data Resources = Resources
    { rTextures   :: AssetManager
    , rFont       :: Maybe SDL.Font.Font
    , rMusicTitle :: Maybe SDL.Mixer.Music
    , rSfxStep    :: Maybe SDL.Mixer.Chunk
    , rSfxDamage  :: Maybe SDL.Mixer.Chunk
    , rSfxDeath   :: Maybe SDL.Mixer.Chunk
    , rSfxAttack  :: Maybe SDL.Mixer.Chunk
    , rSfxMiss    :: Maybe SDL.Mixer.Chunk
    , rSfxPotion  :: Maybe SDL.Mixer.Chunk
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
    entBuffAtkEnd  :: Word32,  --Finaliza el efecto de PotionFueza
    entBuffSpdEnd  :: Word32,  -- Finaliza el efecto de PotionVelocidad
    entInvisible :: Bool,  -- Estado actual de Invisibilidad
    entInvEnd  :: Word32,  -- Finaliza el efecto de PotionInvisibilidad
    entBaseMinAtk :: Int,    -- Mínimo ataque base
    entBaseMaxAtk :: Int,    -- Máximo ataque base
    entBaseSpeed  :: CInt,   -- Velocidad base

    entAttackType  :: AttackType, 
    entAttackTimer :: Word32,

    entDead      :: Bool,
    entDeathTick :: Word32,
    entRegenTick :: Word32,
    entStepTimer :: Word32
} deriving (Show, Eq)

data GameState = GameState {
    player      :: Entity,
    enemies     :: [Entity],
    mapItems    :: [Item],
    gameLog     :: [String],
    resources   :: Resources,
    renderer    :: SDL.Renderer,
    shouldExit  :: Bool,
    gameMode    :: GameMode,
    menuSelection :: Int,
    gameStartTime :: Word32,
    gameOverTimer :: Word32,
    encounteredTypes :: [Clase]
}

type Game = StateT GameState IO
