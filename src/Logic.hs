module Logic where

import qualified SDL
import Linear (V2(..), (^-^), (^+^), (*^))
import Linear.Metric (norm, distance)
import Control.Monad.State
import Control.Monad (when, unless, forM, forM_)
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Data.List (find)
import Linear.Affine (Point(..))
import Config

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

agregarLog :: String -> Game ()
agregarLog msg = do
    liftIO $ putStrLn msg
    modify $ \s -> s { gameLog = take 10 (msg : gameLog s) }

getDirVec :: Direccion -> V2 CInt
getDirVec Arriba    = V2 0 (-1)
getDirVec Abajo     = V2 0 1
getDirVec Izquierda = V2 (-1) 0
getDirVec Derecha   = V2 1 0

btnW, btnH, btnYJugar, btnYSalir, btnX :: CInt
btnW      = 405
btnH      = 112
btnX      = (windowW - btnW) `div` 2
btnYJugar = windowH `div` 2 - 270
btnYSalir = windowH `div` 2 - 145

isClickInside :: V2 CInt -> CInt -> CInt -> CInt -> CInt -> Bool
isClickInside (V2 clickX clickY) rectX rectY rectW rectH =
    clickX >= rectX && clickX <= rectX + rectW &&
    clickY >= rectY && clickY <= rectY + rectH

-- ==========================================
-- 2. COMBATE Y XP
-- ==========================================

atacar :: Word32 -> Game ()
atacar ticks = do
    st <- get
    let pj = player st

    when (ticks > entCooldown pj && not (entDead pj)) $ do
        let dirVec = getDirVec (entDir pj)
        let zonaAtaque = entPos pj + (screenSize *^ dirVec)

        let enemigos = enemies st
        let enemigoGolpeado = find (\e -> entPos e == zonaAtaque && not (entDead e)) enemigos

        case enemigoGolpeado of
            Nothing -> return ()
            Just enemigo -> do
                let dano = randomRango (entMinAtk pj) (entMaxAtk pj) ticks
                let nuevaVida = max 0 (entHp enemigo - dano)
                let estaMuerto = nuevaVida == 0

                let enemigoActualizado = if estaMuerto
                        then enemigo { entHp = 0, entDead = True, entDeathTick = ticks, entAggro = False }
                        else enemigo { entHp = nuevaVida, entAggro = True }

                let otros = filter (/= enemigo) enemigos
                let nuevaLista = enemigoActualizado : otros

                let (pjXp, pjLvlMsg) = if estaMuerto
                                       then (ganarXP pj (entXp enemigo))
                                       else (pj, "")

                let pjFinal = pjXp { entCooldown = ticks + 500 }

                modify $ \s -> s { player = pjFinal, enemies = nuevaLista }

                agregarLog $ "Golpeaste por " ++ show dano ++ " dmg."
                when estaMuerto $ agregarLog $ "¡Enemigo derrotado! +" ++ show (entXp enemigo) ++ " XP."
                when (pjLvlMsg /= "") $ agregarLog pjLvlMsg

ganarXP :: Entity -> Int -> (Entity, String)
ganarXP ent xpGanada =
    let nuevaXp = entXp ent + xpGanada
        necesaria = entNextLevel ent
    in if nuevaXp >= necesaria
       then (ent { entXp = nuevaXp - necesaria
                 , entLevel = entLevel ent + 1
                 , entNextLevel = floor (fromIntegral necesaria * 1.5)
                 , entMaxHp = entMaxHp ent + 5
                 , entHp = entMaxHp ent + 5
                 , entMinAtk = entMinAtk ent + 1
                 , entMaxAtk = entMaxAtk ent + 1
                 }
            , "¡SUBISTE DE NIVEL! (Lvl " ++ show (entLevel ent + 1) ++ ")")
       else (ent { entXp = nuevaXp }, "")

regenerarVida :: Entity -> Word32 -> Entity
regenerarVida ent ticks =
    if not (entDead ent) && entHp ent < entMaxHp ent && ticks > entRegenTick ent + regenTime
    then ent { entHp = min (entMaxHp ent) (entHp ent + 1), entRegenTick = ticks }
    else ent

updateEntityAnimation :: Entity -> Word32 -> Entity
updateEntityAnimation ent ticks =
    let
        animSpeed = 150
        idleFrame = 1
        (newFrame, newTimer) =
            if entIsMoving ent && ticks > entAnimTimer ent + animSpeed
            then
                ((entAnimFrame ent + 1) `mod` fromIntegral heroFramesPerDirection, ticks)
            else if not (entIsMoving ent)
            then
                (idleFrame, entAnimTimer ent)
            else
                (entAnimFrame ent, entAnimTimer ent)
    in ent { entAnimFrame = newFrame, entAnimTimer = newTimer }

-- ==========================================
-- 3. ACTUALIZACIÓN DE ENEMIGOS
-- ==========================================

actualizarEnemigos :: Word32 -> Game ()
actualizarEnemigos ticks = do
    stInicial <- get
    let listaEnemigos = enemies stInicial
    let pj = player stInicial

    nuevosEnemigos <- forM listaEnemigos $ \e -> do
        let eAnim = updateEntityAnimation e ticks

        if entDead eAnim
        then do
            if ticks > entDeathTick eAnim + respawnTime
            then return eAnim { entHp = entMaxHp eAnim, entDead = False, entPos = entOrigin eAnim, entAggro = False }
            else return eAnim
        else do
            let eRegen = regenerarVida eAnim ticks
            let curr = entPos eRegen
            let dest = entPos pj
            let dist = distL1 curr dest

            -- LÓGICA DE VISIÓN SEGÚN TIPO
            let visionRange = case entClass eRegen of
                                Zombie -> zombieAggroRange
                                Vaca   -> cowAggroRange -- Miope
                                _      -> aggroRange

            let tieneAggro = dist < visionRange
            let eConEstado = eRegen { entAggro = tieneAggro }

            eConAccion <- if tieneAggro
            then do
                let diff = dest - curr
                let (V2 dx dy) = diff

                if dist <= attackRange + (screenSize `div` 2)
                then do
                    if ticks > entCooldown eConEstado
                    then do
                        stCurr <- get
                        let pjCurr = player stCurr
                        let dano = randomRango (entMinAtk eConEstado) (entMaxAtk eConEstado) ticks
                        let nuevaVidaPj = max 0 (entHp pjCurr - dano)

                        modify $ \s -> s { player = pjCurr { entHp = nuevaVidaPj } }
                        agregarLog $ "¡El enemigo te ataca! -" ++ show dano ++ " HP"

                        return eConEstado { entCooldown = ticks + enemyAttackInterval }
                    else
                        return eConEstado
                else do
                    if not (entIsMoving eConEstado)
                    then do
                        let stepX = if dx > 0 then V2 1 0 else V2 (-1) 0
                        let stepY = if dy > 0 then V2 0 1 else V2 0 (-1)
                        let moveDirVec = if abs dx > abs dy then stepX else stepY

                        let newDir = vecToDir moveDirVec

                        let nextPos = curr + (screenSize *^ moveDirVec)
                        let chocaEnt = chocaConEntidad nextPos (enemies stInicial) || nextPos == entPos pj

                        if not (esMuro nextPos) && not chocaEnt
                            then return eConEstado { entTarget = nextPos, entIsMoving = True, entDir = newDir }
                            else return eConEstado
                    else return eConEstado
            else do
                if entIsMoving eConEstado
                then return eConEstado
                else do
                    if ticks < entPatrolTimer eConEstado
                    then do
                        let dirVec = getDirVec (entDir eConEstado)
                        let nextPos = curr + (screenSize *^ dirVec)
                        let chocaEnt = chocaConEntidad nextPos (enemies stInicial) || nextPos == entPos pj

                        if not (esMuro nextPos) && not chocaEnt
                        then return eConEstado { entTarget = nextPos, entIsMoving = True }
                        else
                            return eConEstado { entPatrolTimer = 0 }
                    else do
                        let accion = randomRango 0 10 (ticks + fromIntegral (entHp eConEstado))

                        if accion > 2
                        then do
                            let dirRnd = randomRango 0 3 (ticks * 3)
                            let newDir = case dirRnd of
                                            0 -> Arriba
                                            1 -> Abajo
                                            2 -> Izquierda
                                            _ -> Derecha

                            let tiempoCaminar = randomRango 1000 3000 ticks

                            return eConEstado { entDir = newDir, entPatrolTimer = ticks + fromIntegral tiempoCaminar }
                        else do
                            let tiempoEspera = randomRango 500 1500 ticks
                            return eConEstado { entPatrolTimer = ticks + fromIntegral tiempoEspera }

            return eConAccion

    enemigosMovidos <- mapM (\e -> if entIsMoving e && not (entDead e) then updateEntityMovement e else return e) nuevosEnemigos

    modify $ \s -> s { enemies = enemigosMovidos }

vecToDir :: V2 CInt -> Direccion
vecToDir (V2 0 (-1)) = Arriba
vecToDir (V2 0 1)    = Abajo
vecToDir (V2 (-1) 0) = Izquierda
vecToDir _           = Derecha

esMuro :: V2 CInt -> Bool
esMuro (V2 pixelX pixelY) =
    let x = fromIntegral (pixelX `div` screenSize)
        y = fromIntegral (pixelY `div` screenSize)
        fuera = y < 0 || y >= length mapaSuelo || x < 0 || x >= length (head mapaSuelo)
    in if fuera then True else (mapaSuelo !! y !! x) == tilePared

chocaConEntidad :: V2 CInt -> [Entity] -> Bool
chocaConEntidad pos ents = any (\e -> entPos e == pos && not (entDead e)) ents

updateEntityMovement :: Entity -> Game Entity
updateEntityMovement e = do
    let curr = entPos e
    let dest = entTarget e
    let diff = dest ^-^ curr
    let dist = distL1 curr dest
    let speed = entSpeed e

    if dist <= speed
        then return e { entPos = dest, entIsMoving = False }
        else do
            let (V2 dx dy) = diff
            let signumVec = V2 (signum dx) (signum dy)
            let step = speed *^ signumVec
            return e { entPos = curr + step }

isQuitEvent :: SDL.EventPayload -> Bool
isQuitEvent SDL.QuitEvent = True
isQuitEvent (SDL.KeyboardEvent k) = SDL.keyboardEventKeyMotion k == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym k) == SDL.KeycodeQ
isQuitEvent _ = False

isLeftMouseClick :: SDL.EventPayload -> Bool
isLeftMouseClick (SDL.MouseButtonEvent m) =
    SDL.mouseButtonEventMotion m == SDL.Pressed && SDL.mouseButtonEventButton m == SDL.ButtonLeft
isLeftMouseClick _ = False

handleTitleEvents :: [SDL.EventPayload] -> Game ()
handleTitleEvents events = do
    let quit = any isQuitEvent events
    when quit $ modify $ \s -> s { shouldExit = True }

    let mouseClick = find isLeftMouseClick events
    case mouseClick of
        Just (SDL.MouseButtonEvent m) -> do
            let (P (V2 rawX rawY)) = SDL.mouseButtonEventPos m
            let clickPos = V2 (fromIntegral rawX) (fromIntegral rawY)

            if isClickInside clickPos btnX btnYJugar btnW btnH
            then do
                agregarLog "¡Partida Iniciada por clic!"
                modify $ \s -> s { gameMode = Playing }

            else if isClickInside clickPos btnX btnYSalir btnW btnH
            then do
                modify $ \s -> s { shouldExit = True }
            else return ()

        Nothing -> return ()

        _ -> return ()

handlePlayingEvents :: [SDL.EventPayload] -> Word32 -> Game ()
handlePlayingEvents events ticks = do
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
    isSpaceKey (SDL.KeyboardEvent k) = SDL.keyboardEventKeyMotion k == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym k) == SDL.KeycodeSpace
    isSpaceKey _ = False
    checkInput (vec, found) (SDL.KeyboardEvent k)
        | SDL.keyboardEventKeyMotion k == SDL.Pressed =
            case SDL.keysymKeycode (SDL.keyboardEventKeysym k) of
                SDL.KeycodeUp -> (V2 0 (-1), True); SDL.KeycodeDown -> (V2 0 1, True); SDL.KeycodeLeft -> (V2 (-1) 0, True); SDL.KeycodeRight -> (V2 1 0, True); _ -> (vec, found)
    checkInput acc _ = acc
    determineDir (V2 0 (-1)) = Arriba; determineDir (V2 0 1) = Abajo; determineDir (V2 (-1) 0) = Izquierda; determineDir _ = Derecha

handleEvents :: [SDL.EventPayload] -> Word32 -> Game ()
handleEvents events ticks = do
    mode <- gets gameMode
    case mode of
        TitleScreen -> handleTitleEvents events
        Playing     -> handlePlayingEvents events ticks

updateGame :: Word32 -> Game ()
updateGame ticks = do
    mode <- gets gameMode
    when (mode == Playing) $ do
        st <- get
        let pj = player st
        pjMovido <- if entIsMoving pj then updateEntityMovement pj else return pj
        let pjRegen = regenerarVida pjMovido ticks
        let pjAnim = updateEntityAnimation pjRegen ticks
        modify $ \s -> s { player = pjAnim }
        actualizarEnemigos ticks
