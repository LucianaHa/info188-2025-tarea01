-- main.hs
-- TAREA 1: HASKI - Refactorizado con StateT sobre IO

import Control.Monad.State
import System.Exit (exitSuccess)

--------------------------------------------------------------------------------
-- 1. TIPOS DE DATOS DEL JUEGO
--------------------------------------------------------------------------------

-- Tipo de dato con parámetro de tipo (sin cambios, se mantiene puro)
data Personaje a = Personaje {
    nombre :: String,
    clase  :: String,
    poder  :: a,
    vida   :: a
} deriving (Show)

-- FUNCTOR e APPLICATIVE se mantienen (sin cambios, se mantienen puros)
instance Functor Personaje where
    fmap f (Personaje n c p v) = Personaje n c (f p) (f v)

instance Applicative Personaje where
    pure x = Personaje "Invitado" "N/A" x x
    (Personaje _ _ f fv) <*> (Personaje n c p v) = Personaje n c (f p) (fv v)

-- **NUEVO TIPO DE DATO: GameState (El Estado Global)**
-- Contiene todo lo que cambia en el juego.
data GameState = GameState {
    player :: Personaje Int,
    enemy  :: Personaje Int
} deriving (Show)

-- Alias para la Monada del Juego (combina State y IO)
type GameMonad = StateT GameState IO

--------------------------------------------------------------------------------
-- 2. FUNCIONES PURAS DE LÓGICA
--------------------------------------------------------------------------------

-- Crear personaje base
crearPersonaje :: String -> String -> Personaje Int
crearPersonaje nombreJugador claseElegida =
    case claseElegida of
        "Asesino" -> Personaje nombreJugador "Asesino" 12 20
        "Paladin" -> Personaje nombreJugador "Paladin" 10 25
        "Mago"    -> Personaje nombreJugador "Mago" 8 15
        "Guerrero"-> Personaje nombreJugador "Guerrero" 11 22
        "Bardo"   -> Personaje nombreJugador "Bardo" 9 18
        _         -> Personaje nombreJugador "Desconocido" 5 10

-- Aliados posibles (datos puros)
aliados :: [Personaje Int]
aliados = [ Personaje "Guerrero Valiente" "Guerrero" 8 20
          , Personaje "Mago Sabio" "Mago" 6 15
          , Personaje "Bardo Alegre" "Bardo" 5 18
          ]

--------------------------------------------------------------------------------
-- 3. ACCIONES DEL JUEGO (Usan la Monada State)
--------------------------------------------------------------------------------

-- **NOTA:** Usamos liftIO para IO, y get/put/modify para State.

-- Acción de ataque (modifica el estado del enemigo)
accionAtacar :: GameMonad ()
accionAtacar = do
    -- 1. Obtener el estado (Personaje y Enemigo)
    estado <- get
    let pj = player estado
    let lobo = enemy estado

    -- 2. Calcular el nuevo estado del enemigo
    let loboNuevo = lobo { vida = vida lobo - poder pj }

    -- 3. Actualizar el estado con el nuevo enemigo
    put (estado { enemy = loboNuevo })

    -- 4. IO (Mostrar mensaje)
    liftIO $ putStrLn $ nombre pj ++ " ataca al lobo! Vida del lobo: " ++ show (vida loboNuevo)

-- Acción de curación (modifica el estado del jugador)
accionCurar :: GameMonad ()
accionCurar = do
    -- 1. Obtener el estado
    estado <- get
    let pj = player estado

    -- 2. Calcular el nuevo estado del jugador (fmap (+7) se mantiene como item/efecto)
    let pjCurado = fmap (+7) pj

    -- 3. Actualizar el estado con el nuevo jugador
    put (estado { player = pjCurado })

    -- 4. IO (Mostrar mensaje)
    liftIO $ putStrLn $ "Te curas! Vida actual: " ++ show (vida pjCurado)

-- Acción de ayuda (combina estado, IO, y lógica Applicative)
accionAyudar :: GameMonad ()
accionAyudar = do
    liftIO $ putStrLn "\nHas pedido ayuda! Elige a tu futuro aliado:"
    
    -- Mostrar opciones de aliados
    liftIO $ mapM_ (\(i, a) -> putStrLn $ show i ++ ") " ++ nombre a ++ " | Poder: " ++ show (poder a) ++ " | Vida: " ++ show (vida a)) (zip [1..] aliados)
    
    liftIO $ putStrLn "Elige un aliado por número:"
    opcion <- liftIO getLine
    
    let idx = read opcion - 1
    
    if idx >= 0 && idx < length aliados
        then do
            -- 1. Obtener el estado y el aliado elegido
            estado <- get
            let pj = player estado
            let aliadoElegido = aliados !! idx
            
            -- 2. Calcular el nuevo personaje combinado (Uso de Applicative)
            let pjConAliado = (+) <$> pj <*> aliadoElegido
            
            -- 3. Actualizar el estado con el nuevo jugador
            put (estado { player = pjConAliado })
            
            -- 4. IO (Mostrar resultados)
            liftIO $ putStrLn $ "Has elegido a: " ++ nombre aliadoElegido
            liftIO $ putStrLn $ "Poder total combinado: " ++ show (poder pjConAliado)
            liftIO $ putStrLn $ "Vida total combinada: " ++ show (vida pjConAliado)
        else liftIO $ putStrLn "Opción inválida, no se ha unido ningún aliado."

-- Acción de ataque del lobo (modifica el estado del jugador)
accionLoboAtacar :: GameMonad ()
accionLoboAtacar = do
    -- 1. Obtener el estado (Personaje y Enemigo)
    estado <- get
    let pj = player estado
    let lobo = enemy estado

    -- 2. Calcular el nuevo estado del jugador
    let pjNuevo = pj { vida = vida pj - poder lobo }

    -- 3. Actualizar el estado con el nuevo jugador
    put (estado { player = pjNuevo })

    -- 4. IO (Mostrar mensaje)
    liftIO $ putStrLn $ "\n" ++ nombre lobo ++ " ataca a " ++ nombre pj ++ "!"
    liftIO $ putStrLn $ "¡Has recibido " ++ show (poder lobo) ++ " de daño! Vida restante: " ++ show (vida pjNuevo)

--------------------------------------------------------------------------------
-- 4. BUCLE DE JUEGO (La función central que usa la Monada)
--------------------------------------------------------------------------------

-- La función de combate ahora es de tipo GameMonad (), lo que significa que
-- es una computación que puede cambiar el GameState y realizar IO.
combateLoop :: GameMonad ()
combateLoop = do
    -- 1. Obtener el estado actual del juego
    GameState pj lobo <- get
    
    -- 2. Verificar condiciones de fin de juego (Objetivo / Pérdida)
    if vida lobo <= 0
        then liftIO $ putStrLn "¡El lobo ha sido derrotado! Objetivo cumplido."
        else do
            -- 3. Mostrar el menú y leer la acción
            liftIO $ putStrLn "\n--- Turno ---"
            liftIO $ putStrLn $ "Tu Vida: " ++ show (vida pj) ++ " | Vida Lobo: " ++ show (vida lobo)
            liftIO $ putStrLn "Opciones: 1) Atacar | 2) Curar | 3) Pedir Ayuda | 4) Huir"
            accion <- liftIO getLine

            -- 4. Ejecutar la acción elegida del jugador
            case accion of
                "1" -> accionAtacar
                "2" -> accionCurar
                "3" -> accionAyudar
                "4" -> liftIO $ putStrLn (nombre pj ++ " decide huir. Fin del combate.") >> liftIO exitSuccess
                _   -> liftIO $ putStrLn "Opción no válida."

            -- 5. ACCIÓN DEL ENEMIGO: Ataque del lobo.
            if accion `elem` ["1", "2", "3"] 
                then do
                    -- El ataque del lobo se ejecuta sobre el ESTADO actualizado por la acción del jugador.
                    accionLoboAtacar
                    
                    -- **Verificar si el jugador ha sido derrotado DESPUÉS del ataque del lobo**
                    -- Necesitamos obtener el estado nuevamente para la nueva vida del jugador.
                    estadoPostLobo <- get
                    let pjPostLobo = player estadoPostLobo
                    
                    if vida pjPostLobo <= 0
                        then liftIO $ putStrLn $ nombre pjPostLobo ++ " ha sido derrotado. Fin del juego."
                        else combateLoop -- Llama recursivamente si ambos siguen vivos
                
                -- Si la acción fue huir o inválida, no hay ataque del lobo.
                else combateLoop

--------------------------------------------------------------------------------
-- 5. MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== MINI RPG SIMULATOR (State Monad Edition) ==="
    putStrLn "Ingresa tu nombre:"
    nombreJugador <- getLine

    putStrLn "\nElige tu clase: Asesino, Paladin, Mago, Bardo o Guerrero"
    rol <- getLine

    let pjInicial = crearPersonaje nombreJugador rol
    putStrLn ("\nTu personaje base es: " ++ show pjInicial)

    -- Definir el estado inicial
    let loboInicial = Personaje "Lobo Feroz" "Bestia" 12 21
    let estadoInicial = GameState { player = pjInicial, enemy = loboInicial }

    putStrLn "-------------------------"
    putStrLn (nombreJugador ++ " ha sido atacado por un lobo!")
    putStrLn $ "Stats del lobo -> Poder: " ++ show (poder loboInicial) ++ " | Vida: " ++ show (vida loboInicial)
    putStrLn "-------------------------"
    
    -- **EJECUTAR LA MONADA STATE**
    -- runStateT toma la computación GameMonad () y el estado inicial.
    -- Luego devuelve la tupla (resultado_final, estado_final) dentro de IO.
    (resultado, estadoFinal) <- runStateT combateLoop estadoInicial
    
    putStrLn "\n=== JUEGO TERMINADO ==="
    putStrLn $ "Estado final del jugador: " ++ show (player estadoFinal)
    return () -- El resultado 'resultado' es de tipo (), se ignora.