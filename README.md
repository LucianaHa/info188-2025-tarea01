# info188-2025-tarea01
Juego Haskell

instalar:
"""
sudo apt update
sudo apt install ghc libghc-mtl-dev
"""

Luego para ejecutar 
"""
runghc main.hs
"""
o compilar
"""
ghc -o juego main.hs
./juego
"""



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
