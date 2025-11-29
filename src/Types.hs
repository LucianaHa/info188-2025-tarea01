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
import qualified SDL.Mixer as Mixer 
import Data.Map (Map)
import Linear (V2)
import Foreign.C.Types (CInt)
import Data.Word (Word32)
import Control.Monad.State (StateT)

data GameMode = TitleScreen | Playing | GameOver
    deriving (Show, Eq)

data Direccion = Abajo | Izquierda | Derecha | Arriba
    deriving (Show, Eq)

data Clase = Hero | Paladin | Bruja | Chamana | Orco | Esqueleto | Zombie | Vaca | Rata
    deriving (Show, Eq)

data AttackType = NoAttack | AtkNormal | AtkArea
    deriving (Show, Eq)

data ItemType = PotionFuerza | PotionInvisibilidad | PotionVelocidad | PotionVeneno
        deriving (Show, Eq)

data Item = Item {
    itemType :: ItemType,
    itemPos  :: V2 CInt,
    itemObtained :: Bool
} deriving (Show, Eq)

type AssetManager = Map String SDL.Texture

data Resources = Resources
    { rTextures   :: AssetManager
    , rFont       :: Maybe SDL.Font.Font
    
    -- FUSIÓN: Tu mapa de niveles + Los SFX del equipo
    , rMusic      :: Map Int Mixer.Music  -- Música de fondo por nivel
    , rSfxStep    :: Maybe Mixer.Chunk    -- Pasos
    , rSfxDamage  :: Maybe Mixer.Chunk    -- Daño
    , rSfxDeath   :: Maybe Mixer.Chunk    -- Muerte
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
    entBuffAtkEnd  :: Word32,
    entBuffSpdEnd  :: Word32,
    entInvisible :: Bool,
    entInvEnd  :: Word32,
    entBaseMinAtk :: Int,
    entBaseMaxAtk :: Int,
    entBaseSpeed  :: CInt,

    entAttackType  :: AttackType, 
    entAttackTimer :: Word32,

    entDead      :: Bool,
    entDeathTick :: Word32,
    entRegenTick :: Word32,
    
    entStepTimer :: Word32 -- Timer para sonido de pasos
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
    encounteredTypes :: [Clase],
    currentLevel :: Int,
    currentMap   :: [[Int]]
}

type Game = StateT GameState IO