module Logic where


import qualified SDL.Input.Keyboard as Keyboard 
import qualified SDL
import Linear (V2(..), (^-^), (^+^), (*^))
import Linear.Metric (norm, distance, dot)
import Control.Monad.State
import Control.Monad (when, unless, forM, forM_)
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Data.List (find)
import Linear.Affine (Point(..))
import qualified SDL.Mixer as Mixer 
import qualified Data.Map as M

import Types
import Config
import Maps

-- ==========================================
-- 0. CONSTANTES DE COMBATE
-- ==========================================

buffDuration :: Word32
buffDuration = 10000

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

playMusicForLevel :: Int -> Game ()
playMusicForLevel nivel = do
    res <- gets resources
    let musicaMap = rMusic res
    case M.lookup nivel musicaMap of
        Nothing -> return ()
        Just song -> do
            liftIO $ Mixer.haltMusic
            liftIO $ Mixer.playMusic Mixer.Forever song
-- ==========================================
-- 2. COMBATE Y XP
-- ==========================================

createEnemy :: Clase -> V2 CInt -> Entity
createEnemy cls pos = Entity {
    entPos = pos, entTarget = pos, entOrigin = pos, entDir = Abajo,
    entIsMoving = False, entAnimFrame = 0, entAnimTimer = 0,
    entClass = cls,
    
    -- VIDA: Rata(8) < Orco(40) < Vaca(150)
    entHp = case cls of Rata -> 8; Vaca -> 150; Orco -> 40; _ -> 20,
    entMaxHp = case cls of Rata -> 8; Vaca -> 150; Orco -> 40; _ -> 20,
    
    -- DAÑO
    entMinAtk = case cls of Rata -> 1; Vaca -> 8; Orco -> 4; _ -> 2,
    entMaxAtk = case cls of Rata -> 3; Vaca -> 12; Orco -> 7; _ -> 4,
    
    -- VELOCIDAD: El Orco es un poco lento
    entSpeed = case cls of Rata -> 14; Orco -> 6; _ -> 8, 
    
    entXp = case cls of Rata -> 15; Vaca -> 500; Orco -> 100; _ -> 30,
    
    entLevel=1, entNextLevel=0, entCooldown=0, entAggro=False, entPatrolTimer=0,
    entBuffAtkEnd=0, entBuffSpdEnd=0, entInvisible=False, entInvEnd=0,
    entBaseMinAtk=2, entBaseMaxAtk=5, entBaseSpeed=10,
    entDead=False, entDeathTick=0, entRegenTick=0,
    entAttackType=NoAttack, entAttackTimer=0,
    entStepTimer = 0
}

generarEnemigos :: Int -> [Entity]
generarEnemigos nivel = case nivel of
    1 -> [ createEnemy Rata (V2 (x * screenSize) (y * screenSize)) 
         | (x, y) <- [(10, 50), (12, 45), (30, 40), (35, 35), (52, 20), (54, 25)] ] ++
         [ createEnemy Zombie (V2 (12* screenSize) (50* screenSize)) ]
    2 -> [
            createEnemy Orco (V2 (30 * screenSize) (15 * screenSize)),
            createEnemy Vaca (V2 (30 * screenSize) (55 * screenSize)) ]
    _ -> []

checkStairs :: Game ()
checkStairs = do
    st <- get
    let pj = player st
    let mapa = currentMap st
    let (V2 px py) = entPos pj
    let tx = fromIntegral ((px + 32) `div` screenSize)
    let ty = fromIntegral ((py + 32) `div` screenSize)
    let esEscalera = (mapa !! ty !! tx) == tileEscalera
    
    when (currentLevel st == 1 && esEscalera) $ do
        let nuevoNivel = 2
        let nuevosEnemigos = generarEnemigos nuevoNivel
        let nuevoMapa = mapaNivel2
        let startPos = V2 (30 * screenSize) (4 * screenSize) -- Posición corregida
        
        modify $ \s -> s {
            currentLevel = nuevoNivel,
            currentMap = nuevoMapa,
            enemies = nuevosEnemigos,
            player = pj { entPos = startPos, entTarget = startPos, entIsMoving = False },
            gameLog = "¡Has descendido al Nivel 2!" : gameLog s
        }
        playMusicForLevel nuevoNivel

createPlayer :: Clase -> V2 CInt -> Entity
createPlayer cls pos = Entity {
    entPos = pos, entTarget = pos, entOrigin = pos,
    entDir = Abajo, entIsMoving = False, entAnimFrame = 0, entAnimTimer = 0,
    entSpeed = case cls of Chamana -> 15; Bruja -> 11; Paladin -> 9; _ -> 12,
    entClass = cls,
    entMaxHp = case cls of Paladin -> 35; Chamana -> 16; Bruja -> 12; _ -> 20,
    entHp = case cls of Paladin -> 35; Chamana -> 16; Bruja -> 12; _ -> 20,
    entMinAtk = case cls of Bruja -> 9; Chamana -> 5; Paladin -> 4; _ -> 6,
    entMaxAtk = case cls of Bruja -> 14; Chamana -> 7; Paladin -> 6; _ -> 8,
    entCooldown = 0, 
    entBaseSpeed = case cls of Chamana -> 15; Bruja -> 11; Paladin -> 9; _ -> 12,
    entBaseMinAtk = case cls of Bruja -> 9; Chamana -> 5; Paladin -> 4; _ -> 6,
    entBaseMaxAtk = case cls of Bruja -> 14; Chamana -> 7; Paladin -> 6; _ -> 8,
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
        let msg = case cls of
                Orco   -> "¡UN ORCO!..."
                Vaca   -> "¡¡BOSS VACA!!"
                Zombie -> "¡ZOMBIE!..."
                Rata   -> "¡Ratas gigantes!"
                _      -> "¡Enemigo nuevo!"
        agregarLog msg
        modify $ \s -> s { encounteredTypes = cls : encounteredTypes s }


realizarAtaque :: AttackType -> Word32 -> Game ()
realizarAtaque tipo ticks = do
    st <- get
    let pj = player st
    let enemigos = enemies st
    let cooldownBase = if entClass pj == Paladin then 1000 else 800 

    let pjAnim = pj { entAttackType = tipo
                    , entAttackTimer = ticks + atkDuration
                    , entCooldown = ticks + fromIntegral cooldownBase
                    } 
    put st { player = pjAnim }

    let dmgRnd = randomRango (entMinAtk pj) (entMaxAtk pj) ticks
    let (dmgBase, esArea) = case tipo of
            AtkNormal -> (dmgRnd, False)
            AtkArea   -> (max 1 (floor (fromIntegral dmgRnd * 0.6)), True)
            _         -> (0, False)

    let bonus = if entBuffAtkEnd pj > ticks then 5 else 0
    let dmgPotencial = dmgBase + bonus
    let victimas = filter (\e -> not (entDead e) && esGolpeado pj e esArea) enemigos

    -- LÓGICA DE SFX DE ATAQUE (Fusión correcta)
    let res = resources st
    if null victimas 
        then liftIO $ case rSfxMiss res of
                Just sfx -> Mixer.play sfx -- SONIDO FALLO
                Nothing -> return ()
        else liftIO $ case rSfxAttack res of
                Just sfx -> Mixer.play sfx -- SONIDO GOLPE
                Nothing -> return ()

    unless (null victimas) $ do
        forM_ victimas $ \v -> registrarEncuentro (entClass v)
        
        results <- forM enemigos $ \e -> do
            if e `elem` victimas
            then do
                let mult = if not esArea && esBackstab pj e then 1.5 else 1.0
                let dmgTotal = floor (fromIntegral dmgPotencial * mult)
                let nuevaHp = max 0 (entHp e - dmgTotal)
                let muerto = nuevaHp == 0
                let xp = if muerto then entXp e else 0
                
                -- SONIDO DE DAÑO AL ENEMIGO
                stSfx <- get
                let resSfx = resources stSfx
                when (dmgTotal > 0) $ liftIO $ case rSfxDamage resSfx of
                    Just sfx -> Mixer.play sfx 
                    Nothing -> return ()

                let eNew = e { entHp = nuevaHp
                             , entAggro = True
                             , entDead = muerto
                             , entDeathTick = if muerto then ticks else 0 
                             }
                return (eNew, Just dmgTotal, xp)
            else 
                return (e, Nothing, 0)

        let nuevosEnemigos = map (\(e,_,_) -> e) results
        let damages = [ d | (_, Just d, _) <- results ]
        let xpTotal = sum [ x | (_, _, x) <- results ]

        stUpdated <- get
        let pjActualizado = player stUpdated
        let (pjConLevelUp, msgLevel) = if xpTotal > 0 
                                       then ganarXP pjActualizado xpTotal 
                                       else (pjActualizado, "")

        modify $ \s -> s { enemies = nuevosEnemigos, player = pjConLevelUp }

        let dmgString = foldl (\acc d -> if acc == "" then show d else acc ++ "+" ++ show d) "" damages
        let msgHit = if esArea 
                     then "¡Giro! Daño: (" ++ dmgString ++ ")" 
                     else "¡Golpe! Daño: " ++ dmgString

        agregarLog msgHit
        when (msgLevel /= "") $ agregarLog msgLevel

esGolpeado :: Entity -> Entity -> Bool -> Bool
esGolpeado atacante victima esArea
    | esArea = 
        let posA = fmap fromIntegral (entPos atacante)
            posV = fmap fromIntegral (entPos victima)
            dist = distance posA posV :: Float
        in dist < fromIntegral rangeArea
    | otherwise = 
        let dirVec = getDirVec (entDir atacante)
            zonaAtaque = entPos atacante + (screenSize *^ dirVec)
            dist = distL1 zonaAtaque (entPos victima)
        in dist < 40

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
                 
                 -- MEJORAS DE ESTADÍSTICAS
                 , entMaxHp = entMaxHp ent + 10   -- +10 Vida Máxima
                 , entHp = entMaxHp ent + 5      -- Cura total al subir de nivel
                 , entMinAtk = entMinAtk ent + 8 -- +8 Daño Mínimo 
                 , entMaxAtk = entMaxAtk ent + 10 -- +10 Daño Máximo
                 
                 }, "¡LEVEL UP! +HP y +FUERZA")
       else (ent { entXp = nuevaXp }, "")

-- ==========================================
-- 3. ITEMS Y RECOLECCIÓN
pickUpItem :: Word32 -> Game ()
pickUpItem ticks = do
    st <- get
    let pj = player st
    let items = mapItems st

    case find (\i -> itemPos i == entPos pj && not (itemObtained i)) items of
        Nothing -> return ()
        Just itemFound -> do
            let tipo = itemType itemFound
            let itemsRestantes = filter (\i -> itemPos i /= entPos pj) items
            let itemObtenido = itemFound { itemObtained = True }
            let nuevaListaItems = itemObtenido : itemsRestantes

            let (pjEfecto, logMsg) = case tipo of
                    PotionFuerza -> (pj { entBuffAtkEnd = ticks + buffDuration }, "¡Fuerza aumentada!")
                    PotionInvisibilidad -> (pj { entInvEnd = ticks + buffDuration, entInvisible = True }, "¡Eres invisible!")
                    PotionVelocidad -> (pj { entBuffSpdEnd = ticks + buffDuration }, "¡Velocidad aumentada!")
                    PotionVeneno -> (pj { entHp = max 0 (entHp pj - 5) }, "¡Veneno! -5 HP")
                    _ -> (pj, "Item desconocido")

            modify $ \s -> s { player = pjEfecto, mapItems = nuevaListaItems }

            -- SONIDOS DE POCIÓN (Limpio)
            stActual <- get
            let res = resources stActual
            
            if tipo == PotionVeneno 
                then liftIO $ case rSfxDamage res of -- Suena como daño
                        Just sfx -> Mixer.play sfx
                        Nothing -> return ()
                else liftIO $ case rSfxPotion res of -- Suena como powerup
                        Just sfx -> Mixer.play sfx
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
    let animSpeed = 100 
        isAttacking = entAttackType ent /= NoAttack
        shouldAnimate = entIsMoving ent || isAttacking
        (newFrame, newTimer) =
            if shouldAnimate && ticks > entAnimTimer ent + animSpeed
            then ((entAnimFrame ent + 1) `mod` 3, ticks)
            else (if shouldAnimate then entAnimFrame ent else 0, entAnimTimer ent)
    in ent { entAnimFrame = newFrame, entAnimTimer = newTimer }

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
            let pjEsVisible = not (entInvisible pj)
            let visionRange = case entClass eRegen of
                                Zombie -> zombieAggroRange
                                Vaca   -> cowAggroRange
                                _      -> aggroRange

            let tieneAggro = dist < visionRange && pjEsVisible
            let eConEstado = eRegen { entAggro = tieneAggro }

            eConAccion <- if tieneAggro
            then do
                let diff = dest - curr
                let (V2 dx dy) = diff
                if dist <= attackRange + (screenSize `div` 2)
                then do
                    if ticks > entCooldown eConEstado
                    then do
                        registrarEncuentro (entClass eConEstado)

                        when (entClass eConEstado == Vaca) $ do
                             stSfx <- get
                             let res = resources stSfx
                             liftIO $ case rSfxCow res of
                                Just sfx -> Mixer.play sfx
                                Nothing -> return ()

                        stCurr <- get
                        let pjCurr = player stCurr
                        let dano = randomRango (entMinAtk eConEstado) (entMaxAtk eConEstado) ticks
                        let nuevaVidaPj = max 0 (entHp pjCurr - dano)

                        when (dano > 0) $ do
                             let res = resources stCurr
                             liftIO $ case rSfxDamage res of
                                Just sfx -> Mixer.play sfx -- Corregido: Mixer.play
                                Nothing -> return ()

                        if nuevaVidaPj == 0 then do
                            let res = resources stCurr
                            liftIO $ case rSfxDeath res of
                                Just sfx -> Mixer.play sfx -- Corregido: Mixer.play
                                Nothing -> return ()
                            modify $ \s -> s { player = pjCurr { entHp = 0, entDead = True }
                                             , gameMode = GameOver
                                             , gameOverTimer = ticks }
                            agregarLog "¡Has muerto!"
                        else do
                            modify $ \s -> s { player = pjCurr { entHp = nuevaVidaPj } }
                            agregarLog $ "¡El enemigo te ataca! -" ++ show dano ++ " HP"

                        return eConEstado { entCooldown = ticks + enemyAttackInterval }
                    else return eConEstado
                else do
                    if not (entIsMoving eConEstado)
                    then do
                        let stepX = if dx > 0 then V2 1 0 else V2 (-1) 0
                        let stepY = if dy > 0 then V2 0 1 else V2 0 (-1)
                        let moveDirVec = if abs dx > abs dy then stepX else stepY
                        let newDir = vecToDir moveDirVec
                        let nextPos = curr + (screenSize *^ moveDirVec)

                        -- 1. ¿Choca con otros enemigos? (Usamos la lista original para chequear sus Targets)
                        let chocaOtros = isTileBlocked nextPos (filter (/= e) listaEnemigos)
                        -- 2. ¿Choca con el Jugador? (Posición O Target del jugador)
                        let chocaPlayer = nextPos == entPos pj || (entIsMoving pj && nextPos == entTarget pj)

                        let bloqueado = chocaOtros || chocaPlayer || esMuro nextPos (currentMap stInicial)

                        if not bloqueado
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
                        let chocaOtros = isTileBlocked nextPos (filter (/= e) listaEnemigos)
                        let chocaPlayer = nextPos == entPos pj || (entIsMoving pj && nextPos == entTarget pj)
                        let bloqueado = chocaOtros || chocaPlayer || esMuro nextPos (currentMap stInicial)

                        if not bloqueado
                        then return eConEstado { entTarget = nextPos, entIsMoving = True }
                        else return eConEstado { entPatrolTimer = 0 }

                    else do
                        let accion = randomRango 0 10 (ticks + fromIntegral (entHp eConEstado))
                        if accion > 2 
                        then do
                            let dirRnd = randomRango 0 3 (ticks * 3)
                            let newDir = case dirRnd of 0 -> Arriba; 1 -> Abajo; 2 -> Izquierda; _ -> Derecha
                            let tiempoCaminar = randomRango 1000 3000 ticks
                            return eConEstado { entDir = newDir, entPatrolTimer = ticks + fromIntegral tiempoCaminar }
                        else do
                            let tiempoEspera = randomRango 500 1500 ticks
                            return eConEstado { entPatrolTimer = ticks + fromIntegral tiempoEspera }

            return eConAccion

    enemigosMovidos <- mapM (\e -> if entIsMoving e && not (entDead e) then updateEntityMovement e else return e) nuevosEnemigos
    modify $ \s -> s { enemies = enemigosMovidos }

-- ==========================================
-- 5. INPUT HANDLING
-- ==========================================

isQuitEvent :: SDL.EventPayload -> Bool
isQuitEvent SDL.QuitEvent = True
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

    let key1 = isKey SDL.Keycode1 events
    let key2 = isKey SDL.Keycode2 events
    let key3 = isKey SDL.Keycode3 events
    let key4 = isKey SDL.Keycode4 events

    st <- get
    let currentPos = entPos (player st) 

    when key1 $ modify $ \s -> s { player = createPlayer Hero currentPos }
    when key2 $ modify $ \s -> s { player = createPlayer Paladin currentPos }
    when key3 $ modify $ \s -> s { player = createPlayer Bruja currentPos }
    when key4 $ modify $ \s -> s { player = createPlayer Chamana currentPos }

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
    
    -- 1. DETECTAR SALIDA (ESC) Y ATAQUES (Q/W) POR EVENTO (Pulsación única)
    let quit = any isQuitEvent events
    when quit $ modify $ \s -> s { shouldExit = True }

    let keyQ = isKey SDL.KeycodeQ events
    let keyW = isKey SDL.KeycodeW events
    
    -- Solo atacar si no está en cooldown
    when (ticks > entCooldown pj && entAttackType pj == NoAttack) $ do
        if keyQ then realizarAtaque AtkNormal ticks
        else if keyW then realizarAtaque AtkArea ticks
        else return ()

    -- 2. DETECTAR MOVIMIENTO POR ESTADO (Continuo y fluido)
    -- Le preguntamos al hardware si las teclas están apretadas AHORA MISMO
    keys <- liftIO SDL.getKeyboardState
    
    -- Mapeamos teclas a dirección (Usamos Scancodes que son físicos y más rápidos)
    let inputDir = if keys SDL.ScancodeUp    then V2 0 (-1)
              else if keys SDL.ScancodeDown  then V2 0 1
              else if keys SDL.ScancodeLeft  then V2 (-1) 0
              else if keys SDL.ScancodeRight then V2 1 0
              else V2 0 0

    -- 3. EJECUTAR MOVIMIENTO
    -- Si hay input Y (no nos estamos moviendo O acabamos de terminar de movernos)
    unless (entDead pj || entAttackType pj /= NoAttack) $ do
        
        -- Si ya se está moviendo, ignoramos (o podríamos implementar un buffer, pero esto basta)
        unless (entIsMoving pj) $ do
            
            -- Si hay una tecla presionada (vector no es 0,0)
            when (inputDir /= V2 0 0) $ do
                let nuevaDir = determineDir inputDir
                let nextTile = entPos pj + (screenSize *^ inputDir)
                
                -- CAMBIO AQUÍ: Usamos isTileBlocked
                -- Chequeamos si es Muro O si está bloqueada por enemigos (Posición o Target)
                let hayColision = esMuro nextTile (currentMap st) || isTileBlocked nextTile (enemies st)

                if hayColision
                    then 
                        modify $ \s -> s { player = pj { entDir = nuevaDir } }
                    else 
                        modify $ \s -> s { player = pj { 
                            entDir = nuevaDir, 
                            entTarget = nextTile, 
                            entIsMoving = True 
                        } }

  where
    -- Helper para teclas de un solo uso (Ataques)
    isKey code evs = any (\e -> case e of 
        SDL.KeyboardEvent k -> SDL.keyboardEventKeyMotion k == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym k) == code
        _ -> False) evs
        
    -- Helper de dirección
    determineDir (V2 0 (-1)) = Arriba
    determineDir (V2 0 1)    = Abajo
    determineDir (V2 (-1) 0) = Izquierda
    determineDir _           = Derecha


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
resetGame :: Game ()
resetGame = do
    st <- get
    let pjAntiguo = player st
    
    -- 1. RECREAR AL JUGADOR (STATS ORIGINALES)
    -- En lugar de solo curarlo, usamos createPlayer para borrar XP y Nivel.
    -- Mantenemos la clase que eligió el jugador.
    let startPos = V2 (54 * screenSize) (20 * screenSize) -- Posición inicio Nivel 1
    let pjNuevo = createPlayer (entClass pjAntiguo) startPos

    -- 2. REINICIAR ENEMIGOS (NIVEL 1)
    let enemigosNivel1 = generarEnemigos 1

    -- 3. ACTUALIZAR ESTADO COMPLETO
    modify $ \s -> s { 
        player = pjNuevo, 
        enemies = enemigosNivel1, 
        
        -- Volver a Título
        gameMode = TitleScreen, 
        gameLog = ["¡Has muerto! Reiniciando..."],
        
        -- Resetear tiempos
        gameStartTime = 0,
        gameOverTimer = 0, -- Importante resetear esto
        
        -- VOLVER AL NIVEL 1
        currentLevel = 1,
        currentMap = mapaNivel1
    }

    -- 4. RESTAURAR MÚSICA (CRUCIAL)
    playMusicForLevel 1


updateGame :: Word32 -> Game ()
updateGame ticks = do
    -- Eliminado bloque de música roto que dependía de rMusicTitle
    mode <- gets gameMode
    case mode of
        Playing -> do
            stAntes <- get
            let nivelAntes = currentLevel stAntes
            checkStairs 
            stDespues <- get
            let nivelDespues = currentLevel stDespues

            if nivelAntes /= nivelDespues 
            then return () 
            else do
                let pj = player stDespues 
                pjMovido <- if entIsMoving pj then updateEntityMovement pj else return pj
                let pjRegen = regenerarVida pjMovido ticks
                let pjAnim = updateEntityAnimation pjRegen ticks
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

esMuro :: V2 CInt -> [[Int]] -> Bool
esMuro (V2 pixelX pixelY) mapaActual =
    let x = fromIntegral (pixelX `div` screenSize)
        y = fromIntegral (pixelY `div` screenSize)
        h = length mapaActual
        w = length (head mapaActual)
        fuera = y < 0 || y >= h || x < 0 || x >= w
    in if fuera then True else (mapaActual !! y !! x) == tilePared

chocaConEntidad :: V2 CInt -> [Entity] -> Bool
chocaConEntidad pos ents = any (\e -> entPos e == pos && not (entDead e)) ents

isTileBlocked :: V2 CInt -> [Entity] -> Bool
isTileBlocked tile entities = any isBlocking entities
  where
    isBlocking e = 
        not (entDead e) && -- Solo entidades vivas bloquean
        (
           entPos e == tile ||                  -- Está parado ahí
           (entIsMoving e && entTarget e == tile) -- O está CAMINANDO hacia ahí
        )

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
            
            st <- get
            ticks <- liftIO SDL.ticks
            let res = resources st
            let eConSonido = if isPlayerClass (entClass e) && ticks > entStepTimer e
                then e { entStepTimer = ticks + 350 } 
                else e

            when (entStepTimer eConSonido > entStepTimer e) $ do
                liftIO $ case rSfxStep res of
                    Just sfx -> Mixer.play sfx  -- Corregido: Mixer.play
                    Nothing  -> return ()

            return eConSonido { entPos = curr + step }

isPlayerClass :: Clase -> Bool
isPlayerClass Hero    = True
isPlayerClass Paladin = True
isPlayerClass Bruja   = True
isPlayerClass Chamana = True
isPlayerClass _       = False