# info188-2025-tarea01
Juego Haskell

instalar:
"""
sudo apt update
sudo apt install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev libsdl2-mixer-dev
sudo apt install ghc libghc-mtl-dev
"""

Cómo ejecutar tu RPG Gráfico en Linux

Prepara las carpetas:
Asegúrate de que tu carpeta de proyecto tenga esta estructura EXACTA para que el código encuentre las imágenes de prueba. (Puedes usar cualquier imagen .png que tengas, solo renómbrala).

Proyecto/
├── haskirpg.cabal
├── Main.hs
└── Images/
    ├── HUD/
    │   ├── Bars/
    │   │   └── hp_bar.png
    │   ├── WindowSkins/
    │   │   └── default.png
    │   └── TitleScreen/
    │       └── main_title.png
    └── textures2D/
        ├── Animations/
        │   └── hero_walk.png
        └── Tiles/
            └── grass.png


Compila y Ejecuta:
Abre la terminal en la carpeta del proyecto y ejecuta:

cabal run


La primera vez tardará unos minutos porque descargará y compilará SDL2 para Haskell.

Controles:

Usa las Flechas del teclado para mover el cuadrado rojo (o tu personaje si pusiste la imagen hero_walk.png).

Usa Q para salir.


Según gemini nuestro produck Backlog:

📋 Lista de Tareas (Backlog del Equipo)

He dividido el trabajo en 4 grandes áreas. Puedes asignarlas a diferentes "miembros" del equipo o abordarlas secuencialmente.

🛠️ Área 1: Motor y Core (Engine Developer)

El encargado de que "la ventana se abra y se mueva".

    [ ] Configurar SDL2: Crear la función main que abre una ventana de 800x600 y crea un "Renderer".

    [ ] El Game Loop: Implementar el bucle infinito que corre a 60 FPS (Input -> Update -> Render).

    [ ] Sistema de Input: Dejar de usar getLine. Capturar eventos de teclado (KeyDown, KeyUp) para mover al personaje (WASD o Flechas).

    [ ] Carga de Assets: Crear funciones para cargar imágenes .png (spritesheets) y guardarlas en memoria.

🧠 Área 2: Lógica RPG y Sistemas (Systems Designer)

El encargado de las matemáticas y reglas (Tu código actual evoluciona aquí).

    [ ] Refactorizar GameState: Añadir coordenadas (x, y) al Personaje y al Enemigo.

    [ ] Sistema de Experiencia (XP):

        Agregar campos: xpActual, nivel, xpSiguienteNivel.

        Crear función ganarExperiencia :: Int -> Personaje -> Personaje que detecte el "Level Up".

    [ ] Sistema de Habilidades (Skills):

        Crear tipo data Habilidad = Fuego | Hielo | GolpeFuerte.

        Asignar cooldowns (tiempo de espera) y costes de maná.

    [ ] Bestiario (Tipos de Monstruos):

        Crear data TipoElemento = Fuego | Agua | Planta.

        Implementar tabla de debilidades (ej: Agua > Fuego).

🗺️ Área 3: Mundo y Mapas (Level Designer)

El encargado de dónde ocurre el juego.

    [ ] Estructura de Mapa: Definir el mapa no como texto, sino como una cuadrícula (Grid) o Matriz de enteros (Tilemap).

        Ej: 0 = Pasto, 1 = Muro, 2 = Agua.

    [ ] Colisiones: Crear la lógica para que si el jugador intenta moverse a una coordenada que es "Muro", el estado no cambie.

    [ ] Cambio de Niveles: Lógica para que al tocar un punto (ej: una puerta), se cargue una nueva matriz de mapa.

👥 Área 4: NPCs e Interacción (Narrative & AI)

El encargado de dar vida al mundo.

    [ ] Entidad NPC: Crear tipo de dato NPC con nombre, posicion y dialogos.

    [ ] Sistema de Diálogo: Dibujar una caja de texto en la parte inferior de la pantalla cuando el jugador presiona "Espacio" cerca de un NPC.

    [ ] IA Básica de Enemigos: Hacer que el lobo se mueva aleatoriamente o persiga al jugador si está cerca.




Pasos según gemini:

### Explicación de cómo esto usa tus carpetas:

1.  **`Animations/`**:
    * En `Personaje`, tenemos `animacionActual` (Idle, Caminando) y `frameActual`.
    * Cuando integremos gráficos SDL2, tu función de dibujo dirá: *"Si `animacionActual == Caminando`, busca en la carpeta Animations el archivo X y recorta el cuadro número `frameActual`"*.
2.  **`Bars/`**:
    * En `Personaje`, tenemos `hp` y `maxHp`.
    * Tu función de dibujo dirá: `porcentaje = hp / maxHp`. Si es 0.5, dibuja la imagen de la barra llena al 50% de ancho.
3.  **`Facesets/`**:
    * En `Personaje`, el campo `faceSet` es un String `"Facesets/Heroe.png"`. Cuando abras el menú o diálogo, el juego leerá ese string y cargará esa foto.
4.  **`GameOver/` y `TitleScreen/`**:
    * Controlados por `escena :: EscenaJuego`. Si el estado cambia a `GameOver`, el bucle de renderizado deja de dibujar el mapa y dibuja la imagen que tengas en la carpeta GameOver.
5.  **`WindowSkins/`**:
    * En `ConfiguracionUI`, guardamos qué skin se está usando. Al dibujar cajas de texto, usas esa referencia.

### ¿Cuál es el siguiente paso para conectarlo con gráficos?

Ahora que tienes la **Monada State** que controla los *números y estados*, el siguiente paso (cuando integremos SDL2) será crear una función `render` que sea "solo lectura":

###PRompt y Respuesta gemini del uso de monada
Existira una forma de mostrar mas implicito este uso de GameState? La verdad nunca termine de entender bien que es una monada podrías empezar por ahi? y por que GameState es una monada? donde esta declarado eso? o al comportarse como monada esta implicito que lo es

R.
"""
Esta es probablemente la barrera más grande al aprender Haskell, así que vamos a romperla en pedacitos digeribles.

Para entender esto, olvida la palabra "Monada" por un segundo. Pensemos en **patrones de diseño**.

 1\. ¿Qué es realmente una Mónada?

Imagina que estás en una línea de montaje en una fábrica.

  * **Función Normal:** Tomas una pieza de metal, la doblas y la entregas. Entrada -\> Salida.
  * **El Problema:** ¿Qué pasa si la pieza viene rota? ¿O si necesitas anotar en una pizarra "pieza doblada"? ¿O si necesitas leer un manual global antes de doblarla?

Una **Mónada** es simplemente un **envoltorio** (una caja) alrededor de tus datos que añade un "Superpoder" extra a tus funciones.

  * La mónada `Maybe` añade el poder de **fallar** (puede haber dato o nada).
  * La mónada `List` (`[]`) añade el poder de **tener múltiples resultados**.
  * La mónada `State` añade el poder de **pasar una libreta de notas invisible** entre funciones.

> **Respuesta corta:** Una mónada es un patrón para encadenar operaciones que tienen un "efecto secundario" (como guardar estado) sin tener que escribir el código de ese efecto una y otra vez.

-----

 2\. ¿Por qué `GameState` es una mónada?

**Corrección importante:** `GameState` (tu estructura de datos) **NO** es una mónada.
`GameState` es solo la **carga** (el pasajero).

La Mónada es el **Vehículo**. En tu código, el vehículo se llama `StateT`.

Mira esta línea en tu código:

```haskell
type Game = StateT GameState IO
```

  * **`GameState`**: Es la libreta de notas (Datos: posición, vida, etc.).
  * **`StateT`**: Es el **mayordomo invisible** que lleva la libreta.
  * **`Game`**: Es el nombre que le dimos a la combinación "Mayordomo + Libreta".

**¿Dónde está declarado que es una Mónada?**
No lo declaraste tú. Está declarado en la librería que importamos: `Control.Monad.State`.
Dentro de esa librería, alguien muy inteligente escribió algo como esto (simplificado):

```haskell
-- Esto vive dentro de la librería 'mtl' o 'transformers'
instance Monad (State s) where
    return x = ...
    (>>=)    = ...
```

Al hacer `import Control.Monad.State`, automáticamente obtienes el "carnet de Mónada" para `StateT`.

-----

 3\. Haciendo visible lo invisible (El operador `>>=`)

Preguntaste si se puede hacer *más* implícito. La respuesta es: **El bloque `do` YA ES la forma implícita.**

Para que entiendas la "magia", voy a mostrarte cómo se ve tu código **SIN** la notación `do`. Vas a ver cómo la Mónada conecta las tuberías manualmente.

 Versión "Mágica" (Tu código actual con `do`)

Haskell traduce esto automáticamente. Mira qué limpio se ve:

```haskell
moverYAnimar :: Game ()
moverYAnimar = do
    moverPersonaje    -- 1. Modifica posición
    actualizarFrame   -- 2. Modifica frame
    render            -- 3. Usa los datos modificados
```

 Versión "Desazucarada" (Lo que Haskell ve realmente)

Aquí aparece el operador **`>>=`** (se lee "bind" o "entonces"). Este operador es el corazón de la mónada. Toma el estado resultante de la izquierda e inyéctalo en la derecha.

```haskell
moverYAnimar :: Game ()
moverYAnimar = 
    moverPersonaje >>= (\_ ->
        actualizarFrame >>= (\_ ->
            render
        )
    )
```

 Versión "Pesadilla" (Sin usar la Mónada State)

Si no usáramos `StateT` y quisiéramos hacer lo mismo pasando el estado a mano (Programación funcional pura cruda):

```haskell
moverYAnimar :: GameState -> ((), GameState)
moverYAnimar estado1 = 
    let (resultado1, estado2) = moverPersonaje estado1
    in let (resultado2, estado3) = actualizarFrame estado2
       in render estado3
```

**¿Ves la diferencia?**

1.  En la **Versión Pesadilla**, tienes que crear `estado1`, `estado2`, `estado3`... Si te equivocas y pasas `estado1` a `render`, ¡tu personaje pierde el movimiento\!
2.  La **Mónada State (`StateT`)** oculta esa gestión de `estado1 -> estado2 -> estado3`.
3.  El bloque **`do`** oculta el operador `>>=` para que parezca código imperativo (como Python o C).

 4\. ¿Cómo aprovechar esto en tu juego?

Ahora que sabes que la mónada es solo un "transportador de estado automático", puedes usarla para simplificar tu vida.

No necesitas declarar nada nuevo. Solo confía en que si estás dentro de una función que devuelve `Game a`, tienes acceso a:

1.  **Leer:** `st <- get` (Dame la versión más actual de la libreta).
2.  **Escribir:** `put nuevoSt` (Toma esta libreta tachada y úsala de ahora en adelante).
3.  **Modificar:** `modify (\s -> ...)` (Haz un cambio rápido en la libreta).

**Ejemplo Práctico para tu RPG:**

Imagina una función para "Beber Poción".
Sin monada sería complejo actualizar inventario, vida y log. Con monada:

```haskell
beberPocion :: Game ()
beberPocion = do
    -- 1. Curar (Modifica HP)
    modify (\s -> s { playerHp = playerHp s + 50 })
    
    -- 2. Gastar item (Modifica Inventario)
    -- La monada se asegura de que el cambio de HP de arriba NO SE PIERDA
    modify (\s -> s { inventory = quitarPocion (inventory s) })
    
    -- 3. Efecto visual (IO)
    liftIO $ putStrLn "¡Glu glu glu! Recuperaste vida."
```
"""

### Resumen

  * **Monada:** Un patrón para encadenar acciones.
  * **Tu Monada:** `StateT` (El transportador).
  * **Tu Estado:** `GameState` (La carga).
  * **Declaración:** Viene en la librería `mtl`.
  * **Implicito:** El bloque `do` es "azúcar sintáctico" para no tener que pasar variables de estado manualmente una por una.

¿Te ayuda esto a visualizar mejor por qué usamos `Game ()` en todas las funciones?
