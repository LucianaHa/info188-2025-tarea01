module Logic where

import qualified SDL
import Linear (V2(..), (^-^), (^+^), (*^))
import Linear.Metric (norm, distance, dot) -- Importamos dot y distance
import Control.Monad.State
import Control.Monad (when, unless, forM, forM_)
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Data.List (find)
import Linear.Affine (Point(..))
import qualified SDL.Mixer

import Types
import Config
import Maps

-- ==========================================
-- 0. CONSTANTES DE COMBATE
-- ==========================================


buffDuration :: Word32
buffDuration = 10000 -- Duración pociones

-- ==========================================
-- 1. UTILIDADES Y BUFFS
-- ==========================================

checkBuffs :: Word32 -> Game ()
checkBuffs ticks = do
    st <- get
    let pj = player st
    let newPj = checkBuffs' ticks pj
    when (newPj /= pj) $ modify $ \s -> s { player = newPj }

checkBuffs' :: Word32 -> Entity -> Entity
checkBuffs' ticks pj =
    let
        newPjFuerza = if entBuffAtkEnd pj /= 0 && ticks >= entBuffAtkEnd pj
            then pj { entBuffAtkEnd = 0 } else pj

        newPjVelocidad = if entBuffSpdEnd newPjFuerza /= 0
            then if ticks >= entBuffSpdEnd newPjFuerza
                then newPjFuerza { entBuffSpdEnd = 0, entSpeed = 8 }
                else newPjFuerza { entSpeed = 16 }
            else newPjFuerza { entSpeed = 8 }

        newPjInv = if entInvEnd newPjVelocidad /= 0 && ticks >= entInvEnd newPjVelocidad
            then newPjVelocidad { entInvisible = False, entInvEnd = 0 }
            else newPjVelocidad

    in newPjInv

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
btnW = 405; btnH = 112; btnX = (windowW - btnW) `div` 2
btnYJugar = windowH `div` 2 - 270
btnYSalir = windowH `div` 2 - 145

isClickInside :: V2 CInt -> CInt -> CInt -> CInt -> CInt -> Bool
isClickInside (V2 clickX clickY) rectX rectY rectW rectH =
    clickX >= rectX && clickX <= rectX + rectW &&
    clickY >= rectY && clickY <= rectY + rectH

-- ==========================================
-- 2. COMBATE Y XP (NUEVO SISTEMA Q/W)
-- ==========================================

createPlayer :: Clase -> V2 CInt -> Entity
createPlayer cls pos = Entity {
    entPos = pos, entTarget = pos, entOrigin = pos,
    entDir = Abajo, entIsMoving = False, entAnimFrame = 0, entAnimTimer = 0,
    
    -- Stats variables según la clase
    entSpeed = case cls of
        Chamana -> 15 -- Muy rápida
        Bruja   -> 11
        Paladin -> 9  -- Lento
        _       -> 12, -- Hero (Estándar)

    entClass = cls,
    
    entMaxHp = case cls of
        Paladin -> 35 -- Tanque
        Hero    -> 20
        Chamana -> 16
        Bruja   -> 12 -- Frágil
        _       -> 20,
    entHp = case cls of Paladin -> 35; Chamana -> 16; Bruja -> 12; _ -> 20,

    entMinAtk = case cls of Bruja -> 9; Chamana -> 5; Paladin -> 4; _ -> 6,
    entMaxAtk = case cls of Bruja -> 14; Chamana -> 7; Paladin -> 6; _ -> 8,

    -- Cooldown base (velocidad de ataque)
    entCooldown = 0, 
    entBaseSpeed = case cls of Chamana -> 15; Bruja -> 11; Paladin -> 9; _ -> 12,
    entBaseMinAtk = case cls of Bruja -> 9; Chamana -> 5; Paladin -> 4; _ -> 6,
    entBaseMaxAtk = case cls of Bruja -> 14; Chamana -> 7; Paladin -> 6; _ -> 8,

    -- Valores comunes
    entXp = 0, entLevel = 1, entNextLevel = 100,
    entAggro = False, entPatrolTimer = 0,
    entDead = False, entDeathTick = 0, entRegenTick = 0,
    entBuffAtkEnd = 0, entBuffSpdEnd = 0, entInvisible = False, entInvEnd = 0,
    entAttackType = NoAttack, entAttackTimer = 0,
    entStepTimer = 0
}

registrarEncuentro :: Clase -> Game ()
registrarEncuentro cls = do
    st <- get
    let yaVisto = cls `elem` encounteredTypes st
    
    unless yaVisto $ do
        -- Mensajes personalizados según el enemigo
        let msg = case cls of
                Orco   -> "¡UN ORCO! Para mi mala suerte..."
                Vaca   -> "Uh eso es... ¿¿¡¡¿¿UNA VACA!!??"
                Zombie -> "¡ZOMBIE! Es lento pero persistente."
                _      -> "¡Un nuevo enemigo aparece!"
        
        agregarLog msg
        -- Guardamos que ya lo vimos para no repetir el mensaje
        modify $ \s -> s { encounteredTypes = cls : encounteredTypes s }

-- Función principal de ataque (Reemplaza a la antigua 'atacar')
realizarAtaque :: AttackType -> Word32 -> Game ()
realizarAtaque tipo ticks = do
    st <- get
    let pj = player st
    let enemigos = enemies st

    -- 1. Iniciar Animación y Cooldown
    let pjAnim = pj { entAttackType = tipo
                    , entAttackTimer = ticks + atkDuration
                    , entCooldown = ticks + 800
                    } 
    -- Guardamos el estado inicial del ataque
    put st { player = pjAnim }

    -- 2. Definir Daño
    let (dmgBase, esArea) = case tipo of
            AtkNormal -> (damageNormal, False)
            AtkArea   -> (damageArea, True)
            _         -> (0, False)

    let bonus = if entBuffAtkEnd pj > ticks then 5 else 0
    let dmgFinal = dmgBase + bonus

    -- 3. Filtrar enemigos golpeados
    let victimas = filter (\e -> not (entDead e) && esGolpeado pj e esArea) enemigos

    -- 4. Aplicar Daño y ACUMULAR XP
    if null victimas
        then do
            -- CASO A: NO GOLPEASTE A NADIE (MISS)
            let res = resources st
            liftIO $ case rSfxMiss res of
                Just sfx -> SDL.Mixer.play sfx
                Nothing -> return ()
        else do
            let res = resources st
            liftIO $ case rSfxAttack res of
                Just sfx -> SDL.Mixer.play sfx
                Nothing -> return ()

            forM_ victimas $ \v -> registrarEncuentro (entClass v)
            
            -- Usamos mapAccumL o fold para procesar enemigos y sumar XP al mismo tiempo
            -- Pero para hacerlo simple: Procesamos enemigos primero, luego sumamos XP.
            
            -- A) Calcular XP total ganada de los que van a morir
            let xpGanadaTotal = sum [ entXp e | e <- victimas, (entHp e - dmgFinal) <= 0 ]

            -- B) Actualizar lista de enemigos (herirlos o matarlos)
            nuevosEnemigos <- forM enemigos $ \e -> do
                if e `elem` victimas
                then do
                    -- Backstab solo en ataque normal
                    let mult = if not esArea && esBackstab pj e then 1.5 else 1.0
                    let dmgTotal = floor (fromIntegral dmgFinal * mult)
                    let nuevaHp = max 0 (entHp e - dmgTotal)
                    let muerto = nuevaHp == 0
                    
                    return $ e { entHp = nuevaHp
                            , entAggro = True
                            , entDead = muerto
                            , entDeathTick = if muerto then ticks else 0 
                            }
                else return e
                
            -- C) Aplicar la XP al Jugador (¡AQUÍ ESTABA EL ERROR ANTES!)
            -- Si ganamos XP, llamamos a ganarXP, que maneja el Level Up y la Vida Maxima
            stUpdated <- get
            let pjActualizado = player stUpdated -- Recuperamos al pj con animación
            let (pjConLevelUp, msgLevel) = if xpGanadaTotal > 0 
                                        then ganarXP pjActualizado xpGanadaTotal 
                                        else (pjActualizado, "")

            -- D) Guardar TODO en el estado
            modify $ \s -> s { enemies = nuevosEnemigos, player = pjConLevelUp }
            
            -- Logs
            let nVictimas = length victimas
            if esArea 
                then agregarLog $ "¡Giro! Golpeas a " ++ show nVictimas ++ " enemigos."
                else agregarLog $ "¡Golpe! Daño: " ++ show dmgFinal
            
            -- Log de nivel
            when (msgLevel /= "") $ agregarLog msgLevel

-- Detecta si un enemigo es golpeado según el tipo de ataque
esGolpeado :: Entity -> Entity -> Bool -> Bool
esGolpeado atacante victima esArea
    | esArea = 
        -- W: Lógica Circular
        let posA = fmap fromIntegral (entPos atacante)
            posV = fmap fromIntegral (entPos victima)
            dist = distance posA posV :: Float
        in dist < fromIntegral rangeArea
    | otherwise = 
        -- Q: Lógica Frontal (Hitbox permisiva)
        let dirVec = getDirVec (entDir atacante)
            -- Calculamos el centro de la casilla frente al jugador
            zonaAtaque = entPos atacante + (screenSize *^ dirVec)
            -- Calculamos distancia entre esa zona y el enemigo
            dist = distL1 zonaAtaque (entPos victima)
        in dist < 40 -- Tolerancia: Si está a menos de 40px del centro, golpea.


-- Detecta ataque por la espalda (Backstab)
esBackstab :: Entity -> Entity -> Bool
esBackstab atacante victima =
    let vecToAttacker = entPos atacante - entPos victima
        facingDir = getDirVec (entDir victima)
    in (vecToAttacker `dot` facingDir) < 0

ganarXP :: Entity -> Int -> (Entity, String)
ganarXP ent xpGanada =
    let nuevaXp = entXp ent + xpGanada
        necesaria = entNextLevel ent
    in if nuevaXp >= necesaria
       then (ent { entXp = nuevaXp - necesaria
                 , entLevel = entLevel ent + 1
                 , entNextLevel = floor (fromIntegral necesaria * 1.5)
                 , entMaxHp = entMaxHp ent + 5  -- Sube vida máxima
                 , entHp = entMaxHp ent + 5     -- Cura al máximo + bono
                 }, "¡LEVEL UP! HP Aumentada.")
       else (ent { entXp = nuevaXp }, "")

-- ==========================================
-- 3. ITEMS Y RECOLECCIÓN
-- ==========================================

pickUpItem :: Word32 -> Game ()
pickUpItem ticks = do
    st <- get
    let pj = player st
    let items = mapItems st

    -- Buscamos si hay un item en la posición del jugador que NO haya sido recogido
    case find (\i -> itemPos i == entPos pj && not (itemObtained i)) items of
        Nothing -> return ()
        Just itemFound -> do
            let tipo = itemType itemFound
            
            -- Actualizamos la lista de items (marcar como obtenido)
            let itemsRestantes = filter (\i -> itemPos i /= entPos pj) items
            let itemObtenido = itemFound { itemObtained = True }
            let nuevaListaItems = itemObtenido : itemsRestantes

            -- Aplicamos el efecto al jugador
            let (pjEfecto, logMsg) = case tipo of
                    PotionFuerza -> (pj { entBuffAtkEnd = ticks + buffDuration }, "¡Fuerza aumentada!")
                    PotionInvisibilidad -> (pj { entInvEnd = ticks + buffDuration, entInvisible = True }, "¡Eres invisible!")
                    PotionVelocidad -> (pj { entBuffSpdEnd = ticks + buffDuration }, "¡Velocidad aumentada!")
                    PotionVeneno -> (pj { entHp = max 0 (entHp pj - 5) }, "¡Veneno! -5 HP")
                    _ -> (pj, "Item desconocido")

            -- Guardamos los cambios en el estado
            modify $ \s -> s { player = pjEfecto, mapItems = nuevaListaItems }

            -- == SONIDOS ==
            stActual <- get
            let res = resources stActual
            
            if tipo == PotionVeneno 
                then do
                    -- Si es veneno, suena como daño
                    liftIO $ case rSfxDamage res of
                        Just sfx -> SDL.Mixer.play sfx
                        Nothing -> return ()
                else do
                    -- Si es cualquier otra poción, suena el Power-Up
                    liftIO $ case rSfxPotion res of
                        Just sfx -> SDL.Mixer.play sfx
                        Nothing -> return ()

            agregarLog logMsg

-- ==========================================
-- 4. ENTIDADES Y AI
-- ==========================================

regenerarVida :: Entity -> Word32 -> Entity
regenerarVida ent ticks =
    if not (entDead ent) && entHp ent < entMaxHp ent && ticks > entRegenTick ent + regenTime
    then ent { entHp = min (entMaxHp ent) (entHp ent + 1), entRegenTick = ticks }
    else ent

updateEntityAnimation :: Entity -> Word32 -> Entity
updateEntityAnimation ent ticks =
    let animSpeed = 100 -- Velocidad de animación (ms por frame)
        isAttacking = entAttackType ent /= NoAttack
        
        -- Avanzamos el timer si se mueve O si está atacando
        shouldAnimate = entIsMoving ent || isAttacking
        
        (newFrame, newTimer) =
            if shouldAnimate && ticks > entAnimTimer ent + animSpeed
            then ((entAnimFrame ent + 1) `mod` 3, ticks) -- Ciclo de 3 frames para ataques/andar
            else (if shouldAnimate then entAnimFrame ent else 0, entAnimTimer ent) -- Reset a 0 si quieto
            
    in ent { entAnimFrame = newFrame, entAnimTimer = newTimer }


actualizarEnemigos :: Word32 -> Game ()
actualizarEnemigos ticks = do
    stInicial <- get
    let listaEnemigos = enemies stInicial
    let pj = player stInicial

    nuevosEnemigos <- forM listaEnemigos $ \e -> do
        -- 1. Animación (Solo frames de movimiento, ignoramos ataques complejos)
        let eAnim = updateEntityAnimation e ticks

        if entDead eAnim
        then do
            -- Respawn
            if ticks > entDeathTick eAnim + respawnTime
            then return eAnim { entHp = entMaxHp eAnim, entDead = False, entPos = entOrigin eAnim, entAggro = False }
            else return eAnim
        else do
            let eRegen = regenerarVida eAnim ticks
            
            -- Lógica de Aggro (Visión)
            let curr = entPos eRegen
            let dest = entPos pj
            let dist = distL1 curr dest
            let pjEsVisible = not (entInvisible pj)
            
            -- Rangos de visión según tipo
            let visionRange = case entClass eRegen of
                                Zombie -> zombieAggroRange
                                Vaca   -> cowAggroRange
                                _      -> aggroRange

            let tieneAggro = dist < visionRange && pjEsVisible
            let eConEstado = eRegen { entAggro = tieneAggro }

            -- TOMA DE DECISIONES
            eConAccion <- if tieneAggro
            then do
                -- == MODO PERSECUCIÓN / ATAQUE ==
                let diff = dest - curr
                let (V2 dx dy) = diff

                -- Si está en rango de ataque (cuerpo a cuerpo simple)
                if dist <= attackRange + (screenSize `div` 2)
                then do
                    if ticks > entCooldown eConEstado
                    then do
                        registrarEncuentro (entClass eConEstado)
                        -- Daño simple directo al jugador
                        stCurr <- get
                        let pjCurr = player stCurr
                        let dano = randomRango (entMinAtk eConEstado) (entMaxAtk eConEstado) ticks
                        let nuevaVidaPj = max 0 (entHp pjCurr - dano)

                        --REPRODUCIR SONIDO SI HAY DAÑO
                        when (dano > 0) $ do
                             let res = resources stCurr
                             liftIO $ case rSfxDamage res of
                                Just sfx -> SDL.Mixer.play sfx
                                Nothing -> return ()

                        if nuevaVidaPj == 0 then do
                            let res = resources stCurr
                            liftIO $ case rSfxDeath res of
                                Just sfx -> SDL.Mixer.play sfx
                                Nothing -> return ()
                            modify $ \s -> s { player = pjCurr { entHp = 0, entDead = True }
                                             , gameMode = GameOver
                                             , gameOverTimer = ticks }
                            agregarLog "¡Has muerto!"
                        else do
                            modify $ \s -> s { player = pjCurr { entHp = nuevaVidaPj } }
                            agregarLog $ "¡El enemigo te ataca! -" ++ show dano ++ " HP"

                        return eConEstado { entCooldown = ticks + enemyAttackInterval }
                    else
                        return eConEstado
                else do
                    -- Perseguir caminando
                    if not (entIsMoving eConEstado)
                    then do
                        let stepX = if dx > 0 then V2 1 0 else V2 (-1) 0
                        let stepY = if dy > 0 then V2 0 1 else V2 0 (-1)
                        -- Moverse en el eje más lejano
                        let moveDirVec = if abs dx > abs dy then stepX else stepY
                        let newDir = vecToDir moveDirVec
                        let nextPos = curr + (screenSize *^ moveDirVec)
                        
                        -- Chequear colisiones
                        let chocaEnt = chocaConEntidad nextPos (enemies stInicial) || nextPos == entPos pj

                        if not (esMuro nextPos) && not chocaEnt
                            then return eConEstado { entTarget = nextPos, entIsMoving = True, entDir = newDir }
                            else return eConEstado
                    else return eConEstado
            else do
                -- == MODO PATRULLA / IDLE (Restaurado) ==
                if entIsMoving eConEstado
                then return eConEstado
                else do
                    if ticks < entPatrolTimer eConEstado
                    then do
                        -- Si sigue en timer de patrulla, intentar moverse a donde mira
                        let dirVec = getDirVec (entDir eConEstado)
                        let nextPos = curr + (screenSize *^ dirVec)
                        let chocaEnt = chocaConEntidad nextPos (enemies stInicial) || nextPos == entPos pj

                        if not (esMuro nextPos) && not chocaEnt
                        then return eConEstado { entTarget = nextPos, entIsMoving = True }
                        else return eConEstado { entPatrolTimer = 0 } -- Chocó, resetear timer
                    else do
                        -- IA Aleatoria: Decidir si caminar o esperar
                        let accion = randomRango 0 10 (ticks + fromIntegral (entHp eConEstado))

                        if accion > 2 -- 70% probabilidad de caminar
                        then do
                            let dirRnd = randomRango 0 3 (ticks * 3)
                            let newDir = case dirRnd of
                                            0 -> Arriba; 1 -> Abajo; 2 -> Izquierda; _ -> Derecha
                            let tiempoCaminar = randomRango 1000 3000 ticks
                            return eConEstado { entDir = newDir, entPatrolTimer = ticks + fromIntegral tiempoCaminar }
                        else do
                            -- Esperar quieto
                            let tiempoEspera = randomRango 500 1500 ticks
                            return eConEstado { entPatrolTimer = ticks + fromIntegral tiempoEspera }

            return eConAccion

    -- Aplicar movimiento real
    enemigosMovidos <- mapM (\e -> if entIsMoving e && not (entDead e) then updateEntityMovement e else return e) nuevosEnemigos

    modify $ \s -> s { enemies = enemigosMovidos }


-- ==========================================
-- 5. INPUT HANDLING (Q/W IMPLEMENTADO)
-- ==========================================

isQuitEvent :: SDL.EventPayload -> Bool
isQuitEvent SDL.QuitEvent = True
-- CAMBIA LA Q POR ESCAPE:
isQuitEvent (SDL.KeyboardEvent k) = 
    SDL.keyboardEventKeyMotion k == SDL.Pressed && 
    SDL.keysymKeycode (SDL.keyboardEventKeysym k) == SDL.KeycodeEscape
isQuitEvent _ = False


isLeftMouseClick :: SDL.EventPayload -> Bool
isLeftMouseClick (SDL.MouseButtonEvent m) =
    SDL.mouseButtonEventMotion m == SDL.Pressed && SDL.mouseButtonEventButton m == SDL.ButtonLeft
isLeftMouseClick _ = False

handleTitleEvents :: [SDL.EventPayload] -> Word32 -> Game () 
handleTitleEvents events ticks = do
    let quit = any isQuitEvent events
    when quit $ modify $ \s -> s { shouldExit = True }

    -- DETECTAR TECLAS 1, 2, 3, 4 PARA CAMBIAR CLASE
    let key1 = isKey SDL.Keycode1 events
    let key2 = isKey SDL.Keycode2 events
    let key3 = isKey SDL.Keycode3 events
    let key4 = isKey SDL.Keycode4 events

    st <- get
    let currentPos = entPos (player st) -- Mantenemos la posición original

    when key1 $ modify $ \s -> s { player = createPlayer Hero currentPos }
    when key2 $ modify $ \s -> s { player = createPlayer Paladin currentPos }
    when key3 $ modify $ \s -> s { player = createPlayer Bruja currentPos }
    when key4 $ modify $ \s -> s { player = createPlayer Chamana currentPos }

    -- DETECTAR CLIC EN JUGAR
    let mouseClick = find isLeftMouseClick events
    case mouseClick of
        Just (SDL.MouseButtonEvent m) -> do
            let (P (V2 rawX rawY)) = SDL.mouseButtonEventPos m
            let clickPos = V2 (fromIntegral rawX) (fromIntegral rawY)
            if isClickInside clickPos btnX btnYJugar btnW btnH
                then modify $ \s -> s { gameMode = Playing, gameStartTime = ticks }
                else if isClickInside clickPos btnX btnYSalir btnW btnH
                     then modify $ \s -> s { shouldExit = True }
                     else return ()
        _ -> return ()
  where
    isKey code evs = any (\e -> case e of 
        SDL.KeyboardEvent k -> SDL.keyboardEventKeyMotion k == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym k) == code
        _ -> False) evs

handlePlayingEvents :: [SDL.EventPayload] -> Word32 -> Game ()
handlePlayingEvents events ticks = do
    st <- get
    let pj = player st
    
    -- 1. Detectar Quit
    let quit = any isQuitEvent events
    when quit $ modify $ \s -> s { shouldExit = True }

    -- 2. Detectar Ataques (Q y W)
    let keyQ = isKey SDL.KeycodeQ events
    let keyW = isKey SDL.KeycodeW events
    
    -- Ejecutar ataque si no está en cooldown y no está atacando ya
    when (ticks > entCooldown pj && entAttackType pj == NoAttack) $ do
        if keyQ then realizarAtaque AtkNormal ticks
        else if keyW then realizarAtaque AtkArea ticks
        else return ()

    -- 3. Movimiento (Flechas)
    -- Solo mover si NO está atacando (para que se quede quieto al pegar)
    unless (entIsMoving pj || entDead pj || entAttackType pj /= NoAttack) $ do
        let (inputDir, hasInput) = foldl checkInput (V2 0 0, False) events
        when hasInput $ do
            let nuevaDir = determineDir inputDir
            -- Si cambia de dirección, solo girar
            if nuevaDir /= entDir pj
                then modify $ \s -> s { player = pj { entDir = nuevaDir } }
                else do
                    -- Si mantiene dirección, avanzar
                    let nextTile = entPos pj + (screenSize *^ inputDir)
                    unless (esMuro nextTile || chocaConEntidad nextTile (enemies st)) $ do
                        modify $ \s -> s { player = pj { entTarget = nextTile, entIsMoving = True } }

  where
    -- Helper limpio para detectar teclas
    isKey code evs = any (\e -> case e of 
        SDL.KeyboardEvent k -> SDL.keyboardEventKeyMotion k == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym k) == code
        _ -> False) evs
        
    checkInput (vec, found) (SDL.KeyboardEvent k)
        | SDL.keyboardEventKeyMotion k == SDL.Pressed =
            case SDL.keysymKeycode (SDL.keyboardEventKeysym k) of
                SDL.KeycodeUp -> (V2 0 (-1), True)
                SDL.KeycodeDown -> (V2 0 1, True)
                SDL.KeycodeLeft -> (V2 (-1) 0, True)
                SDL.KeycodeRight -> (V2 1 0, True)
                _ -> (vec, found)
    checkInput acc _ = acc
    determineDir (V2 0 (-1)) = Arriba; determineDir (V2 0 1) = Abajo; determineDir (V2 (-1) 0) = Izquierda; determineDir _ = Derecha

handleEvents :: [SDL.EventPayload] -> Word32 -> Game ()
handleEvents events ticks = do
    mode <- gets gameMode
    case mode of
        TitleScreen -> handleTitleEvents events ticks 
        
        Playing     -> handlePlayingEvents events ticks
        GameOver    -> return ()
-- ==========================================
-- 6. UPDATE LOOP
-- ==========================================
-- En src/Logic.hs

resetGame :: Game ()
resetGame = do
    st <- get
    let pj = player st
    
    -- 1. Resetear al Jugador
    let pjReset = pj { 
        entHp = entMaxHp pj, 
        entDead = False, 
        entPos = entOrigin pj, 
        entTarget = entOrigin pj, 
        entIsMoving = False,
        entAttackType = NoAttack, -- Aseguramos que no empiece atacando
        entAttackTimer = 0
    }

    -- 2. Resetear a los Enemigos
    let enemiesReset = map (\e -> e { 
        entHp = entMaxHp e, 
        entDead = False, 
        entPos = entOrigin e, 
        entAggro = False,
        entAttackType = NoAttack,
        entAttackTimer = 0
    }) (enemies st)

    -- 3. Guardar todo en el estado (Incluyendo gameStartTime = 0)
    modify $ \s -> s { 
        player = pjReset, 
        enemies = enemiesReset, 
        gameMode = TitleScreen, 
        gameLog = ["¡Nueva Partida!"],
        gameStartTime = 0 
    }
updateGame :: Word32 -> Game ()
updateGame ticks = do
    -- Preguntamos si la música sigue sonando
    musicPlaying <- SDL.Mixer.playingMusic
    
    unless musicPlaying $ do
        -- Si se detuvo, la forzamos a arrancar de nuevo
        st <- get
        let res = resources st
        case rMusicTitle res of -- Usamos rMusicTitle porque es la única que tenemos cargada
            Just mus -> SDL.Mixer.playMusic SDL.Mixer.Forever mus
            Nothing  -> return ()
    mode <- gets gameMode
    case mode of
        Playing -> do
            st <- get
            let pj = player st
            
            -- Movimiento
            pjMovido <- if entIsMoving pj then updateEntityMovement pj else return pj
            let pjRegen = regenerarVida pjMovido ticks
            let pjAnim = updateEntityAnimation pjRegen ticks
            
            -- RESETEAR ESTADO DE ATAQUE SI ACABÓ LA ANIMACIÓN
            let pjFinal = if entAttackType pjAnim /= NoAttack && ticks >= entAttackTimer pjAnim
                          then pjAnim { entAttackType = NoAttack }
                          else pjAnim
            
            modify $ \s -> s { player = pjFinal }
            
            pickUpItem ticks
            checkBuffs ticks
            actualizarEnemigos ticks

        GameOver -> do
            st <- get
            if ticks > gameOverTimer st + 5000 then resetGame else return ()
        _ -> return ()

-- Helpers faltantes
vecToDir :: V2 CInt -> Direccion
vecToDir (V2 0 (-1)) = Arriba; vecToDir (V2 0 1) = Abajo; vecToDir (V2 (-1) 0) = Izquierda; vecToDir _ = Derecha

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
            -- LÓGICA DE MOVIMIENTO
            let (V2 dx dy) = diff
            let signumVec = V2 (signum dx) (signum dy)
            let step = speed *^ signumVec
            
            -- LÓGICA DE SONIDO DE PASOS
            st <- get
            ticks <- liftIO SDL.ticks
            let res = resources st
            
            -- ¿Es el Héroe? ¿Ha pasado suficiente tiempo (350ms)?
            let eConSonido = if isPlayerClass (entClass e) && ticks > entStepTimer e
                then e { entStepTimer = ticks + 350 } -- Resetear timer (350ms de espera)
                else e

            -- Si acabamos de resetear el timer, reproducir sonido
            when (entStepTimer eConSonido > entStepTimer e) $ do
                liftIO $ case rSfxStep res of
                    Just sfx -> SDL.Mixer.play sfx -- ¡PLOC!
                    Nothing  -> return ()

            return eConSonido { entPos = curr + step }

isPlayerClass :: Clase -> Bool
isPlayerClass Hero    = True
isPlayerClass Paladin = True
isPlayerClass Bruja   = True
isPlayerClass Chamana = True
isPlayerClass _       = False