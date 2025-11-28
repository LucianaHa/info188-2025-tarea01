module Maps where

-- ==========================================
-- 1. IDs DE TUS TILES
-- ==========================================
-- Usamos los números que encontraste:
tileSuelo :: Int
tileSuelo = 14 

tilePared :: Int
tilePared = 7   

tileAgua :: Int
tileAgua  = -1  

-- ==========================================
-- 2. HERRAMIENTAS DE EDICIÓN
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
-- 3. GENERADOR DE MAZMORRA GRANDE (60x60)
-- ==========================================

mapDim :: Int
mapDim = 60 -- ¡Importante! El mapa debe ser grande para que la pos (54,20) exista

mapaSuelo :: [[Int]]
mapaSuelo = 
    let 
        -- 1. BASE: Todo lleno de PAREDES
        base = replicate mapDim (replicate mapDim tilePared)
        
        -- 2. SALA A (Circular Izquierda Abajo)
        salaA1 = replaceRows 42 58 (replaceInRow 6 18 tileSuelo) base
        salaA2 = replaceRows 40 42 (replaceInRow 8 16 tileSuelo) salaA1
        salaA3 = replaceRows 58 60 (replaceInRow 8 16 tileSuelo) salaA2

        -- 3. SALA B (Rectángulo Izquierda Arriba)
        salaB = replaceRows 5 20 (replaceInRow 6 16 tileSuelo) salaA3

        -- 4. PASILLO VERTICAL (Conecta A y B)
        pasilloAB = replaceRows 20 40 (replaceInRow 9 13 tileSuelo) salaB

        -- 5. SALA C (Central Grande)
        salaC = replaceRows 30 50 (replaceInRow 25 45 tileSuelo) pasilloAB

        -- 6. PASILLO HORIZONTAL (Conecta A y C)
        pasilloAC = replaceRows 46 50 (replaceInRow 18 25 tileSuelo) salaC

        -- 7. SALA D (Derecha Grande - Final) - AQUÍ EMPIEZA EL JUGADOR (54, 20)
        salaD = replaceRows 10 35 (replaceInRow 50 58 tileSuelo) pasilloAC
        
        -- 8. PASILLO LABERINTO (Conecta C y D)
        pasilloCD1 = replaceRows 30 35 (replaceInRow 45 54 tileSuelo) salaD 
        pasilloCD2 = replaceRows 30 55 (replaceInRow 50 54 tileSuelo) pasilloCD1

    in pasilloCD2

-- Dejamos la capa de objetos vacía
mapaObjetos :: [[Int]]
mapaObjetos = replicate mapDim (replicate mapDim (-1))