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

-- Distancia Manhattan
distL1 :: V2 CInt -> V2 CInt -> CInt
distL1 (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- Pseudo-Random simple
randomRango :: Int -> Int -> Word32 -> Int
randomRango minVal maxVal seed = 
    let r = fromIntegral (seed `mod` 100) -- 0 a 99
        range = maxVal - minVal + 1
    in minVal + (r `mod` range)

-- ==========================================
-- 2. COLISIONES
-- ==========================================

esMuro :: V2 CInt -> Bool
esMuro (V2 pixelX pixelY) = 
    let x = fromIntegral (pixelX `div` screenSize)
        y = fromIntegral (pixelY `div` screenSize)
        
        fueraDeMapa = y < 0 || y >= length mapaSuelo || 
                      x < 0 || x >= length (head mapaSuelo)
    in if fueraDeMapa 
       then True
       else 
           let tileID = mapaSuelo !! y !! x
               esPared = tileID == tilePared
           in esPared

chocaConEntidad :: V2 CInt -> [Entity] -> Bool
chocaConEntidad pos listaEntidades = 
    any (\e -> entPos e == pos && entHp e > 0) listaEntidades

-- ==========================================
-- 3. COMBATE
-- ==========================================

atacar :: Word32 -> Game ()
atacar ticks = do
    st <- get
    let pj = player st
    
    when (ticks > entCooldown pj) $ do
        let dirVec = case entDir pj of
                        Arriba    -> V2 0 (-1)
                        Abajo     -> V2 0 1
                        Izquierda -> V2 (-1) 0
                        Derecha   -> V2 1 0
        let zonaAtaque = entPos pj + (screenSize *^ dirVec)
        
        let enemigos = enemies st
        let enemigoGolpeado = find (\e -> entPos e == zonaAtaque && entHp e > 0) enemigos
        
        case enemigoGolpeado of
            Nothing -> return ()
            Just enemigo -> do
                let dano = randomRango (entMinAtk pj) (entMaxAtk pj) ticks
                let nuevaVida = max 0 (entHp enemigo - dano)
                let enemigoNuevo = enemigo { entHp = nuevaVida, entAggro = True }
                
                let otrosEnemigos = filter (/= enemigo) enemigos
                let nuevaLista = if nuevaVida > 0 
                                 then enemigoNuevo : otrosEnemigos 
                                 else otrosEnemigos
                
                let pjNuevo = pj { entCooldown = ticks + 500 }
                
                put $ st { player = pjNuevo, enemies = nuevaLista }
                liftIO $ putStrLn $ "Golpe: " ++ show dano ++ " dmg. Vida restante: " ++ show nuevaVida

-- ==========================================
-- 4. IA ENEMIGA (TURNO)
-- ==========================================

actualizarEnemigos :: Word32 -> Game ()
actualizarEnemigos ticks = do
    st <- get
    let pj = player st
    let listaEnemigos = enemies st
    
    nuevosEnemigos <- forM listaEnemigos $ \e -> do
        if entHp e > 0 && not (entIsMoving e) && entAggro e
            then do
                -- IA SIMPLE CORREGIDA
                let diff = entPos pj - entPos e
                let dist = distL1 (entPos pj) (entPos e)
                
                -- Desempaquetamos el vector Diferencia para leer X e Y
                let (V2 dx dy) = diff

                if dist <= screenSize
                    then do
                         when (ticks > entCooldown e) $ do
                             let dano = randomRango (entMinAtk e) (entMaxAtk e) ticks
                             let nuevaVidaPj = max 0 (entHp pj - dano)
                             modify $ \s -> s { player = pj { entHp = nuevaVidaPj } }
                             liftIO $ putStrLn $ "Te golpearon! Vida: " ++ show nuevaVidaPj
                         return e { entCooldown = ticks + 1000 }
                    else do
                         -- Moverse (Solo ortogonal)
                         -- Usamos dx y dy directamente (sin ^. _x)
                         let stepX = if dx > 0 then V2 1 0 else V2 (-1) 0
                         let stepY = if dy > 0 then V2 0 1 else V2 0 (-1)
                         
                         let moveDir = if abs dx > abs dy then stepX else stepY
                         let nextPos = entPos e + (screenSize *^ moveDir)
                         
                         if not (esMuro nextPos) && not (chocaConEntidad nextPos listaEnemigos) && nextPos /= entPos pj
                             then return e { entTarget = nextPos, entIsMoving = True, entDir = dirFromVec moveDir }
                             else return e
            else do
                if entIsMoving e
                    then updateEntityMovement e
                    else return e

    put $ st { enemies = nuevosEnemigos }

  where
    dirFromVec (V2 0 (-1)) = Arriba
    dirFromVec (V2 0 1)    = Abajo
    dirFromVec (V2 (-1) 0) = Izquierda
    dirFromVec _           = Derecha

updateEntityMovement :: Entity -> Game Entity
updateEntityMovement e = do
    let curr = entPos e
    let dest = entTarget e
    let diff = dest ^-^ curr
    let dist = distL1 curr dest
    
    if dist <= walkSpeed
        then return e { entPos = dest, entIsMoving = False }
        else do
            -- Corrección de tipos anterior (Escalar * Vector)
            let step = walkSpeed *^ signumVec diff
            return e { entPos = curr + step }
    where
        signumVec (V2 x y) = V2 (signumInt x) (signumInt y)
        signumInt x | x > 0 = 1 | x < 0 = -1 | otherwise = 0

-- ==========================================
-- 5. UPDATE GENERAL
-- ==========================================

handleEvents :: [SDL.EventPayload] -> Word32 -> Game ()
handleEvents events ticks = do
    st <- get
    let pj = player st
    let quit = any isQuitEvent events
    when quit $ modify $ \s -> s { shouldExit = True }

    let attackKey = any isSpaceKey events
    when attackKey $ atacar ticks

    unless (entIsMoving pj) $ do
        let (inputDir, hasInput) = foldl checkInput (V2 0 0, False) events
        when hasInput $ do
            let nuevaDir = determineDir inputDir
            if nuevaDir /= entDir pj
                then modify $ \s -> s { player = pj { entDir = nuevaDir } }
                else do
                    let current = entPos pj
                    let nextTile = current + (screenSize *^ inputDir)
                    
                    let chocaEnemigo = chocaConEntidad nextTile (enemies st)
                    
                    unless (esMuro nextTile || chocaEnemigo) $ do
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
                SDL.KeycodeUp    -> (V2 0 (-1), True)
                SDL.KeycodeDown  -> (V2 0 1, True)
                SDL.KeycodeLeft  -> (V2 (-1) 0, True)
                SDL.KeycodeRight -> (V2 1 0, True)
                _                -> (vec, found)
    checkInput acc _ = acc

    determineDir (V2 0 (-1)) = Arriba
    determineDir (V2 0 1)    = Abajo
    determineDir (V2 (-1) 0) = Izquierda
    determineDir (V2 1 0)    = Derecha
    determineDir _           = Abajo

updateGame :: Word32 -> Game ()
updateGame ticks = do
    st <- get
    -- 1. Mover Jugador
    pjMovido <- if entIsMoving (player st) then updateEntityMovement (player st) else return (player st)
    modify $ \s -> s { player = pjMovido }
    
    -- 2. Mover Enemigos
    actualizarEnemigos ticks