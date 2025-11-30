# info188-2025-tarea01
# Haski RPG: La Venganza de la Vaca

**Asignatura:** Programación en Paradigmas (INFO188)
**Profesor:** Cristóbal A. Navarro
**Fecha:** 29 de Noviembre 2025

## Integrantes del Grupo
* [Jonatan Aguero]
* [Mayra Carrillo]
* [Luciana Habert]
* [Alen Rupailaf]
* [Cristobal Veas]
---
## 1. Descripción del Proyecto

**Objetivo Principal:**
El jugador está encerrado en una mazmorra y busca sobrevivir a hordas de Orcos y Zombies, acumular experiencia para subir de nivel y finalmente derrotar a la "Vaca Sagrada", el jefe final del nivel para poder escapar y salir a la superficie..

---

## 2. Instrucciones de Instalación y Ejecución

El proyecto utiliza `Cabal` para la gestión de paquetes y requiere las librerías de desarrollo de SDL2.

### Requisitos del Sistema (Linux/Ubuntu/Debian)
Ejecute los siguientes comandos en su terminal para instalar las dependencias necesarias:

```bash
sudo apt update
sudo apt install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev libsdl2-mixer-dev
sudo apt install ghc libghc-mtl-dev
```

Cómo ejecutar tu RPG Gráfico en Linux

Prepara las carpetas:
Asegúrate de que tu carpeta de proyecto tenga esta estructura EXACTA para que el código encuentre las imágenes de prueba. (Puedes usar cualquier imagen .png que tengas, solo renómbrala).
```
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
        │   ├── cow-white.png
        │   ├── hero.png
        │   ├── ogre.png
        │   ├── shield.png
        │   ├── zombie.png
        │   └── bladesHit.png
        └── Tiles/
            └── dungeon.png
```

Compila y Ejecuta:
Abre la terminal en la carpeta del proyecto y ejecuta:
"""
cabal run
"""

La primera vez tardará unos minutos porque descargará y compilará SDL2 para Haskell.

Controles:

Usa las Flechas del teclado para mover el cuadrado rojo (o tu personaje si pusiste la imagen hero_walk.png).
Version 0.0.0.0.0.12
Registro de Cambios (Changelog) ULTIMO PARCHE

A continuación se detallan las modificaciones, mejoras y nuevas funcionalidades implementadas sobre la versión base del proyecto.

1. Nuevas Mecánicas de Combate

    Sistema de Daño Direccional (Backstab):

        Se implementó lógica vectorial (Producto Punto) para detectar la orientación de los combatientes.

        Daño Frontal: x1.0 (Daño normal).

        Daño por la Espalda: x1.5 (Crítico).

    Habilidades Activas:

        Tecla Q (Ataque Normal): Golpe frontal de corto alcance con hitbox permisiva.

        Tecla W (Ataque en Área): Golpe circular (360°) que daña a múltiples enemigos cercanos con daño reducido.

    Mecánica de Escudo:

        El jugador ahora posee un escudo visual que indica su dirección de bloqueo.

        El escudo rota dinámicamente según la dirección de la mirada (Arriba, Abajo, Izquierda, Derecha).

2. Mejoras Gráficas y Renderizado (2.5D)

    Escalado de Sprites (Upscaling):

        Se modificó el motor de renderizado para separar la lógica de colisión (64px) del tamaño visual.

        Héroe y Enemigos: Ahora se renderizan a 96px/128px (visual) manteniendo la casilla lógica de 64px, creando un efecto de profundidad (los pies se alinean a la base de la casilla y la cabeza sobresale).

    Corrección de Spritesheets:

        Implementación de lectura dinámica de sprites según la entidad (32px para Héroe/Zombies, 48px para Vaca/Jefes) para evitar glitches visuales.

    Efectos Visuales (VFX):

        Escudo de Energía: Renderizado semi-transparente (Alpha Blending) con ajuste de posición dinámico (baja al mirar al frente para no tapar la cara, sube al mirar atrás).

        Efectos de Espada: Se añadieron animaciones de "corte" y "giro" (hoja verde) al ejecutar ataques con Q y W.

3. Interfaz de Usuario (HUD)

    Panel de Estadísticas: Se agregó un HUD en la esquina superior izquierda que muestra en tiempo real:

        Vida Actual / Máxima.

        Nivel Actual.

        Experiencia (XP) acumulada / Siguiente Nivel.

    Guía de Controles: Texto en pantalla recordando las teclas de acción (Q, W) y mecánicas clave.

    Mensajes Temporales: Sistema de mensajes flotantes (ej: "Sube Nivel y Derrota a la Vaca") que aparecen al inicio del juego y desaparecen automáticamente tras 5 segundos.

    Logs de Combate: Se mejoró el feedback en el log de texto para indicar daño crítico y número de enemigos golpeados por área.





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

## PRompt y Respuesta gemini del uso de monada
Existira una forma de mostrar mas implicito este uso de GameState? La verdad nunca termine de entender bien que es una monada podrías empezar por ahi? y por que GameState es una monada? donde esta declarado eso? o al comportarse como monada esta implicito que lo es

R.
```
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
### Resumen

  * **Monada:** Un patrón para encadenar acciones.
  * **Tu Monada:** `StateT` (El transportador).
  * **Tu Estado:** `GameState` (La carga).
  * **Declaración:** Viene en la librería `mtl`.
  * **Implicito:** El bloque `do` es "azúcar sintáctico" para no tener que pasar variables de estado manualmente una por una.

¿Te ayuda esto a visualizar mejor por qué usamos `Game ()` en todas las funciones?
---

## Créditos y Referencias

Este proyecto fue desarrollado integrando conocimientos de la asignatura con herramientas modernas de desarrollo y recursos de arte libre.

### Herramientas y Desarrollo
* **Asistencia de IA:** Se utilizó **Google Gemini** como herramienta de apoyo para la comprensión profunda de conceptos teóricos (como la Mónada State), la depuración de errores de tipo en Haskell y la optimización de algoritmos de combate, cumpliendo con el uso responsable de IA sugerido en la pauta.
* **Lenguaje:** [Haskell](https://www.haskell.org/) (GHC 8.8.4).

### Recursos Gráficos (Arte)
* **Sprites y Tilesets:** Los recursos visuales, incluyendo los sprites animados de los personajes (Héroe, Bruja, Chamana, Paladín), los enemigos (Orco, Zombie, Vaca) y los tiles del mapa, fueron obtenidos de la librería de assets de la aplicación gratuita **RPG Paper Maker**.
* **Motor Gráfico:** Renderizado realizado mediante la librería [SDL2](https://www.libsdl.org/) y sus bindings para Haskell (`sdl2`, `sdl2-image`, `sdl2-ttf`).

### Recursos de Audio
* **Música:** El tema de la pantalla de título ("Title Screen") fue compuesto por Irrational Machines, parte del "RPG Title Screen Music Pack" obtenido en OpenGameArt.org (Licencia CC-BY 4.0).

* **Efectos de Sonido (SFX):** La colección de efectos sonoros (pasos, ataques, daño e interacciones) proviene del paquete "Minifantasy - Dungeon SFX Pack" creado por Leohpaz, disponible en Itch.io.

* **Motor de Audio:** Reproducción gestionada mediante la librería SDL2-Mixer.
