module Maps (
    tileSuelo,
    tilePared,
    tileAgua,
    tileEscalera,
    tilePuerta,
    mapaNivel1,   -- Exportar esto
    mapaNivel2,   -- Exportar esto
    getFloorPositions,
    replaceInRow,
    replaceRows,
    tileEscalera,
    mapaNivel1,
    mapaNivel2
) where

import Linear (V2(..))
import Foreign.C.Types (CInt)
import Config (screenSize)

-- ==========================================
-- 1. PON AQUÍ TUS NUEVOS IDs ENCONTRADOS
-- ==========================================
tileSuelo :: Int
tileSuelo = 14

tileEscalera :: Int
tileEscalera = 5

tilePared :: Int
tilePared = 7

tileAgua :: Int
tileAgua  = -1

tilePuerta :: Int
tilePuerta = 50

-- ==========================================
-- 2. HERRAMIENTAS
-- ==========================================
replaceInRow :: Int -> Int -> Int -> [Int] -> [Int]
replaceInRow start end val row =
    take start row ++ replicate (end - start) val ++ drop end row

replaceRows :: Int -> Int -> ([Int] -> [Int]) -> [[Int]] -> [[Int]]
replaceRows start end rowFunc matriz =
    take start matriz ++
    map rowFunc (take (end - start) (drop start matriz)) ++
    drop end matriz

-- ==========================================
-- 3. GENERADOR DE MAZMORRA (60x60)
-- ==========================================
mapDim :: Int
mapDim = 60

mapaSuelo :: [[Int]]
mapaSuelo =
    let
        base = replicate mapDim (replicate mapDim tilePared)

        -- Sala A (Circular)
        salaA1 = replaceRows 42 58 (replaceInRow 6 18 tileSuelo) base
        salaA2 = replaceRows 40 42 (replaceInRow 8 16 tileSuelo) salaA1
        salaA3 = replaceRows 58 60 (replaceInRow 8 16 tileSuelo) salaA2

        -- Sala B
        salaB = replaceRows 5 20 (replaceInRow 6 16 tileSuelo) salaA3

        -- Pasillo AB
        pasilloAB = replaceRows 20 40 (replaceInRow 9 13 tileSuelo) salaB

        -- Sala C
        salaC = replaceRows 30 50 (replaceInRow 25 45 tileSuelo) pasilloAB

        -- Pasillo AC
        pasilloAC = replaceRows 46 50 (replaceInRow 18 25 tileSuelo) salaC

        -- Sala D
        salaD = replaceRows 10 35 (replaceInRow 50 58 tileSuelo) pasilloAC

        -- Pasillo CD
        pasilloCD1 = replaceRows 30 35 (replaceInRow 45 54 tileSuelo) salaD
        pasilloCD2 = replaceRows 30 55 (replaceInRow 50 54 tileSuelo) pasilloCD1

    in pasilloCD2

mapaNivel1 :: [[Int]]
mapaNivel1 =
    let base = mapaSuelo -- Tu laberinto actual
        mapaConEscalera = replaceRows 10 11 (replaceInRow 10 11 tileEscalera) base
    in mapaConEscalera

mapaNivel2 :: [[Int]]
mapaNivel2 =
    let base = replicate mapDim (replicate mapDim tilePared) -- Todo Pared

        -- 1. EL PASILLO ANCHO (Ancho 7)
        -- Va desde arriba (Y=5) hasta la sala del jefe (Y=50)
        -- X: del 27 al 34 (34 - 27 = 7 bloques de ancho)
        pasillo = replaceRows 5 50 (replaceInRow 27 34 tileSuelo) base

        -- 2. SALA 1 (10x10) - Inicio
        -- Y: 10 a 20, X: 25 a 35
        sala1 = replaceRows 10 20 (replaceInRow 25 35 tileSuelo) pasillo

        -- 3. SALA 2 (10x10) - Medio
        -- Y: 25 a 35, X: 25 a 35
        sala2 = replaceRows 25 35 (replaceInRow 25 35 tileSuelo) sala1

        -- 4. SALA 3 (10x10) - Antesala
        -- Y: 40 a 50, X: 25 a 35
        sala3 = replaceRows 40 50 (replaceInRow 25 35 tileSuelo) sala2

        -- 5. SALA DEL JEFE (12x12) - Final
        -- Y: 50 a 62 (Hasta el fondo), X: 24 a 36
        salaJefeBase = replaceRows 50 62 (replaceInRow 24 36 tileSuelo) sala3

        salaConPuerta = replaceRows 61 62 (replaceInRow 30 31 tilePuerta) salaJefeBase

    in salaConPuerta

mapaObjetos :: [[Int]]
mapaObjetos = replicate mapDim (replicate mapDim (-1))

-- ==========================================
-- 4. UTILIDADES DEL MAPA (NUEVO)
-- ==========================================

-- Lista de todas las coordenadas de casillas de suelo (en PIXELES)
getFloorPositions :: [[Int]] -> [V2 CInt]
getFloorPositions matriz =
    [ V2 (fromIntegral x * screenSize) (fromIntegral y * screenSize)
    | (y, row) <- zip [0..] matriz
    , (x, tileID) <- zip [0..] row
    , tileID == tileSuelo -- tileSuelo es 14 según tus snippets
    ]
