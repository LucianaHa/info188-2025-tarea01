module Logic where

import qualified SDL
import Linear (V2(..), (^-^), (^+^), (*^))
import Linear.Metric (norm, distance)
import Control.Monad.State
import Control.Monad (when, unless, forM, forM_)
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Data.List (find)

import Types
import Config
import Maps

-- ==========================================
-- 1. UTILIDADES
-- ==========================================

distL1 :: V2 CInt -> V2 CInt -> CInt
distL1 (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

randomRango :: Int -> Int -> Word32 -> Int
randomRango minVal maxVal seed = 
    let r = fromIntegral (seed `mod` 100)
        range = maxVal - minVal + 1
    in minVal + (r `mod` range)

-- NUEVO: Agregar mensaje al Log
agregarLog :: String -> Game ()
agregarLog msg = do
    liftIO $ putStrLn msg -- Seguimos imprimiendo en consola por si acaso
    modify $ \s -> s { gameLog = take 10 (msg : gameLog s) } -- Guardamos los últimos 10

-- ==========================================
-- 2. COMBATE Y XP
-- ==========================================

atacar :: Word32 -> Game ()
atacar ticks = do
    st <- get
    let pj = player st
    
    when (ticks > entCooldown pj && not (entDead pj)) $ do
        let dirVec = case entDir pj of
                        Arriba    -> V2 0 (-1)
                        Abajo     -> V2 0 1
                        Izquierda -> V2 (-1) 0
                        Derecha   -> V2 1 0
        let zonaAtaque = entPos pj + (screenSize *^ dirVec)
        
        let enemigos = enemies st
        -- Solo atacamos enemigos VIVOS
        let enemigoGolpeado = find (\e -> entPos e == zonaAtaque && not (entDead e)) enemigos
        
        case enemigoGolpeado of
            Nothing -> return ()
            Just enemigo -> do
                let dano = randomRango (entMinAtk pj) (entMaxAtk pj) ticks
                let nuevaVida = max 0 (entHp enemigo - dano)
                let estaMuerto = nuevaVida == 0
                
                -- Actualizar Enemigo (Daño, Aggro o Muerte)
                let enemigoActualizado = if estaMuerto
                        then enemigo { entHp = 0, entDead = True, entDeathTick = ticks, entAggro = False }
                        else enemigo { entHp = nuevaVida, entAggro = True }
                
                -- Actualizar Lista
                let otros = filter (/= enemigo) enemigos
                let nuevaLista = enemigoActualizado : otros
                
                -- Actualizar Jugador (Cooldown y XP si mató)
                let (pjXp, pjLvlMsg) = if estaMuerto 
                                       then (ganarXP pj (entXp enemigo))
                                       else (pj, "")
                                       
                let pjFinal = pjXp { entCooldown = ticks + 500 }
                
                modify $ \s -> s { player = pjFinal, enemies = nuevaLista }
                
                agregarLog $ "Golpeaste por " ++ show dano ++ " dmg."
                when estaMuerto $ agregarLog $ "¡Enemigo derrotado! +" ++ show (entXp enemigo) ++ " XP."
                when (pjLvlMsg /= "") $ agregarLog pjLvlMsg

-- Sistema de Nivel
ganarXP :: Entity -> Int -> (Entity, String)
ganarXP ent xpGanada =
    let nuevaXp = entXp ent + xpGanada
        necesaria = entNextLevel ent
    in if nuevaXp >= necesaria
       then -- LEVEL UP!
            (ent { entXp = nuevaXp - necesaria
                 , entLevel = entLevel ent + 1
                 , entNextLevel = floor (fromIntegral necesaria * 1.5)
                 , entMaxHp = entMaxHp ent + 5
                 , entHp = entMaxHp ent + 5 -- Curar al subir nivel
                 , entMinAtk = entMinAtk ent + 1
                 , entMaxAtk = entMaxAtk ent + 1
                 }
            , "¡SUBISTE DE NIVEL! (Lvl " ++ show (entLevel ent + 1) ++ ")")
       else (ent { entXp = nuevaXp }, "")

-- ==========================================
-- 3. LOGICA DE JUEGO (REGEN, RESPAWN, IA)
-- ==========================================

-- Regeneración pasiva de vida
regenerarVida :: Entity -> Word32 -> Entity
regenerarVida ent ticks =
    if not (entDead ent) && entHp ent < entMaxHp ent && ticks > entRegenTick ent + regenTime
    then ent { entHp = min (entMaxHp ent) (entHp ent + 1), entRegenTick = ticks }
    else ent

-- Lógica de Enemigos (IA + Respawn)
actualizarEnemigos :: Word32 -> Game ()
actualizarEnemigos ticks = do
    stInicial <- get
    let listaEnemigos = enemies stInicial
    let pj = player stInicial -- Leemos el jugador inicial para cálculos de distancia
    
    nuevosEnemigos <- forM listaEnemigos $ \e -> do
        if entDead e
        then do
            -- LOGICA DE RESPAWN
            if ticks > entDeathTick e + respawnTime
            then return e { entHp = entMaxHp e, entDead = False, entPos = entOrigin e, entAggro = False }
            else return e -- Sigue muerto
        else do
            -- LOGICA DE IA (Solo si está vivo)
            -- 1. Regenerar
            let eRegen = regenerarVida e ticks
            
            -- 2. Perseguir/Atacar si tiene Aggro
            if entAggro eRegen
            then do
                let curr = entPos eRegen
                let dest = entPos pj
                let diff = dest - curr
                let dist = distL1 curr dest
                let (V2 dx dy) = diff

                if dist <= screenSize
                then do
                    -- ATACAR
                    when (ticks > entCooldown eRegen) $ do
                        stCurr <- get -- Obtenemos estado actual para dañar al jugador real
                        let pjCurr = player stCurr
                        let dano = randomRango (entMinAtk eRegen) (entMaxAtk eRegen) ticks
                        let nuevaVidaPj = max 0 (entHp pjCurr - dano)
                        
                        modify $ \s -> s { player = pjCurr { entHp = nuevaVidaPj } }
                        agregarLog $ "¡Te golpearon! -" ++ show dano ++ " HP"
                    
                    return eRegen { entCooldown = ticks + 1000 }
                else do
                    -- MOVER (Solo si no está atacando/moviéndose)
                    if not (entIsMoving eRegen)
                    then do
                        let stepX = if dx > 0 then V2 1 0 else V2 (-1) 0
                        let stepY = if dy > 0 then V2 0 1 else V2 0 (-1)
                        let moveDir = if abs dx > abs dy then stepX else stepY
                        let nextPos = curr + (screenSize *^ moveDir)
                        
                        -- Chequear colisiones
                        let chocaEnt = chocaConEntidad nextPos (enemies stInicial) || nextPos == entPos pj
                        if not (esMuro nextPos) && not chocaEnt
                            then return eRegen { entTarget = nextPos, entIsMoving = True }
                            else return eRegen
                    else return eRegen -- Ya se está moviendo
            else return eRegen -- No tiene aggro

    -- Actualizar físicas de movimiento de los enemigos vivos y moviéndose
    enemigosMovidos <- mapM (\e -> if entIsMoving e && not (entDead e) then updateEntityMovement e else return e) nuevosEnemigos
    
    modify $ \s -> s { enemies = enemigosMovidos }

-- Colisiones
esMuro :: V2 CInt -> Bool
esMuro (V2 pixelX pixelY) = 
    let x = fromIntegral (pixelX `div` screenSize)
        y = fromIntegral (pixelY `div` screenSize)
        fuera = y < 0 || y >= length mapaSuelo || x < 0 || x >= length (head mapaSuelo)
    in if fuera then True else (mapaSuelo !! y !! x) == tilePared

chocaConEntidad :: V2 CInt -> [Entity] -> Bool
chocaConEntidad pos ents = any (\e -> entPos e == pos && not (entDead e)) ents

-- Movimiento Suave
updateEntityMovement :: Entity -> Game Entity
updateEntityMovement e = do
    let curr = entPos e
    let dest = entTarget e
    let diff = dest ^-^ curr
    let dist = distL1 curr dest
    
    if dist <= walkSpeed
        then return e { entPos = dest, entIsMoving = False }
        else do
            let (V2 dx dy) = diff
            let signumVec = V2 (signum dx) (signum dy)
            let step = walkSpeed *^ signumVec
            return e { entPos = curr + step }

-- ==========================================
-- 4. UPDATE GENERAL
-- ==========================================

handleEvents :: [SDL.EventPayload] -> Word32 -> Game ()
handleEvents events ticks = do
    st <- get
    let pj = player st
    let quit = any isQuitEvent events
    when quit $ modify $ \s -> s { shouldExit = True }

    let attackKey = any isSpaceKey events
    when attackKey $ atacar ticks

    unless (entIsMoving pj || entDead pj) $ do
        let (inputDir, hasInput) = foldl checkInput (V2 0 0, False) events
        when hasInput $ do
            let nuevaDir = determineDir inputDir
            if nuevaDir /= entDir pj
                then modify $ \s -> s { player = pj { entDir = nuevaDir } }
                else do
                    let nextTile = entPos pj + (screenSize *^ inputDir)
                    unless (esMuro nextTile || chocaConEntidad nextTile (enemies st)) $ do
                        modify $ \s -> s { player = pj { entTarget = nextTile, entIsMoving = True } }
  where
    isQuitEvent SDL.QuitEvent = True
    isQuitEvent (SDL.KeyboardEvent k) = SDL.keyboardEventKeyMotion k == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym k) == SDL.KeycodeQ
    isQuitEvent _ = False
    isSpaceKey (SDL.KeyboardEvent k) = SDL.keyboardEventKeyMotion k == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym k) == SDL.KeycodeSpace
    isSpaceKey _ = False
    checkInput (vec, found) (SDL.KeyboardEvent k) 
        | SDL.keyboardEventKeyMotion k == SDL.Pressed = 
            case SDL.keysymKeycode (SDL.keyboardEventKeysym k) of
                SDL.KeycodeUp -> (V2 0 (-1), True); SDL.KeycodeDown -> (V2 0 1, True); SDL.KeycodeLeft -> (V2 (-1) 0, True); SDL.KeycodeRight -> (V2 1 0, True); _ -> (vec, found)
    checkInput acc _ = acc
    determineDir (V2 0 (-1)) = Arriba; determineDir (V2 0 1) = Abajo; determineDir (V2 (-1) 0) = Izquierda; determineDir _ = Derecha

updateGame :: Word32 -> Game ()
updateGame ticks = do
    st <- get
    
    -- 1. Actualizar Jugador (Movimiento + Regen)
    let pj = player st
    pjMovido <- if entIsMoving pj then updateEntityMovement pj else return pj
    let pjRegen = regenerarVida pjMovido ticks
    
    modify $ \s -> s { player = pjRegen }
    
    -- 2. Actualizar Enemigos
    actualizarEnemigos ticks