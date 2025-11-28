module Maps where

-- ==========================================
-- 1. PON AQUÍ TUS NUEVOS IDs ENCONTRADOS
-- ==========================================
tileSuelo :: Int
tileSuelo = 14 

tilePared :: Int
tilePared = 7  

tileAgua :: Int
tileAgua  = -1  

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

mapaObjetos :: [[Int]]
mapaObjetos = replicate mapDim (replicate mapDim (-1))