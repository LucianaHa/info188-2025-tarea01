# Haski RPG: La Venganza de la Vaca

**Asignatura:** Programación en Paradigmas Funcional y Paralelo (INFO188)

**Profesor:** Cristóbal Navarro

**Fecha:** 29 de noviembre 2025

## Integrantes del Grupo

* Jonatan Aguero
* Mayra Carrillo
* Luciana Habert
* Alen Rupailaf
* Cristóbal Veas

## 1. Descripción del Proyecto

**Objetivo Principal:** El jugador despierta encerrado en una mazmorra oscura y debe sobrevivir a hordas de criaturas hostiles (orcos, ratas gigantes, zombies). El objetivo es acumular experiencia, fortalecer al personaje y descender a través de los niveles hasta encontrar y derrotar a la "Vaca Sagrada", el jefe final que custodia la salida a la superficie.

## 2. Instrucciones de Instalación

Este proyecto utiliza **Cabal** para la gestión de dependencias y **SDL2** para el motor gráfico y de audio. Para facilitar la configuración del entorno Haskell, se recomienda el uso de **GHCup**.

### Paso 1: Instalación de Dependencias del Sistema

Es necesario instalar las librerías de desarrollo de SDL2 en el sistema operativo antes de compilar el proyecto.

#### Opción A: Ubuntu / Debian / Mint

```
sudo apt update
sudo apt install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
sudo apt install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev libsdl2-mixer-dev
```

#### Opción B: Arch Linux / Manjaro

En distribuciones basadas en Arch, las cabeceras de desarrollo suelen estar incluidas en los paquetes principales.

```
sudo pacman -Syu
sudo pacman -S base-devel curl
sudo pacman -S sdl2 sdl2_image sdl2_ttf sdl2_mixer
```

### Paso 2: Instalación de Haskell vía GHCup

Si no dispone de Haskell instalado, utilice GHCup para configurar el compilador (GHC) y el gestor de paquetes (Cabal). Ejecute el siguiente comando y siga las instrucciones en pantalla:

```
curl --proto '=https' --tlsv1.2 -sSf [https://get-ghcup.haskell.org](https://get-ghcup.haskell.org) | sh
```

Tras la instalación, reinicie su terminal (o ejecute `source ~/.ghcup/env`) e instale las versiones recomendadas:

```
ghcup install ghc recommended
ghcup set ghc recommended
ghcup install cabal recommended
```

### Paso 3: Estructura del Directorio

Verifique que la carpeta del proyecto mantenga la siguiente estructura para asegurar la correcta carga de módulos y recursos (imágenes/audio):

```
Proyecto/
├── src/                  <-- Módulos de lógica y motor
│   ├── Assets.hs
│   ├── Config.hs
│   ├── Logic.hs
│   ├── Maps.hs
│   ├── Render.hs
│   └── Types.hs
├── Images/               <-- Sprites, Tilesets e Interfaz
├── Music/                <-- Archivos de Audio y SFX
├── assets/               <-- Fuentes de letras y música
├── haskirpg.cabal        <-- Configuración del proyecto
├── Makefile              <-- Comandos de automatización
├── Main.hs               <-- Archivo principal (Entry Point)
└── README.md
```

## 3. Compilación y Ejecución

Se incluye un `Makefile` para simplificar los comandos de Cabal. Desde la raíz del proyecto:

* **Compilar y Jugar:**
  ```
  make run
  ```
  *(Equivalente a: `cabal run`)*

* **Solo Compilar:**
  ```
  make build
  ```

* **Modo de Compatibilidad (WSL):**
  Si utiliza *Windows Subsystem for Linux* y experimenta problemas de audio, utilice:
  ```
  make run-wsl
  ```

> **Nota:** La primera compilación puede tomar varios minutos mientras Cabal descarga y compila las dependencias de SDL2.

## 4. Controles y Guía de Juego

### Controles Básicos

* **Flechas de Dirección:** Mover al personaje por el mapa.
* **Q:** Ataque Normal (Golpe rápido frontal).
* **W:** Ataque Especial (Giro en área, ideal para multitudes).
* **1-4:** Cambiar de clase (Solo en pantalla de título).
* **ESC:** Salir del juego.

### Sistema de Objetos

Durante la exploración, el jugador encontrará pociones con diversos efectos:

* **Fuerza:** Aumenta el daño temporalmente.
* **Velocidad:** Aumenta la velocidad de movimiento temporalmente.
* **Invisibilidad:** Los enemigos dejan de perseguirte temporalmente.
* **Veneno:** Daña al jugador al contacto.

## 5. Características del Juego

El proyecto implementa diversas mecánicas avanzadas utilizando programación funcional:

### 1. Sistema de Combate Táctico

* **Mecánica de "Backstab":** El juego calcula vectores de dirección entre el atacante y la víctima. Atacar a un enemigo por la espalda otorga un bonificador de daño de **x1.5**.
* **Cooldowns:** Las habilidades poseen tiempos de enfriamiento, obligando al jugador a gestionar sus ataques estratégicamente.
* **Escudo Direccional:** Un indicador visual muestra hacia dónde está bloqueando el personaje.

### 2. Motor Gráfico 2.5D

* **Renderizado por Capas:** Se separa la lógica de colisión (hitbox) del renderizado visual.
* **Efecto de Profundidad:** Los sprites de los personajes son más altos que las casillas del suelo y se dibujan con un ordenamiento Z (depth sorting) para crear la ilusión de profundidad y superposición correcta.
* **Transparencias:** Uso de *Alpha Blending* para efectos de invisibilidad y elementos de la interfaz.

### 3. Inteligencia Artificial (IA)

* **Patrones de Comportamiento:** Los enemigos tienen estados de patrulla aleatoria y estados de persecución ("Aggro") basados en la distancia y línea de visión.
* **Jefes:** La "Vaca Sagrada" posee estadísticas y comportamientos diferenciados (mayor rango de visión y daño).

### 4. Interfaz de Usuario (HUD)

* **Feedback Visual:** Barras de vida flotantes sobre las entidades heridas.
* **Log de Eventos:** Panel de texto que narra el combate y la recolección de objetos en tiempo real.
* **Estadísticas:** Visualización constante de HP, XP y Nivel.

## 6. Conceptos Teóricos: Uso de la Mónada State

Para gestionar la complejidad del estado del juego en un paradigma puramente funcional, se utilizó el patrón de **Mónada State**.

* **`GameState`**: Estructura inmutable que contiene el estado del mundo (jugador, enemigos, mapa).
* **`Game` Monad**: Definida como `type Game = StateT GameState IO`. Esto permite encapsular el estado y pasarlo implícitamente entre funciones, a la vez que permite efectos secundarios (IO) como renderizar gráficos o reproducir sonidos.
* **Ventaja:** Permite escribir código imperativo dentro de bloques `do` (ej: `get`, `put`, `modify`) sin perder la pureza referencial y seguridad de tipos de Haskell.

## 7. Créditos

### Desarrollo y Herramientas

* **Lenguaje:** Haskell (GHC).
* **Librerías:** SDL2, SDL2-Image, SDL2-Mixer, SDL2-TTF, Linear.
* **Asistencia Técnica:** Consultas puntuales sobre tipos y configuración de entorno realizadas a modelos LLM (Google Gemini).

### Recursos Artísticos

* **Gráficos:** Sprites y Tilesets provenientes de la librería de *RPG Paper Maker*.
* **Música:** *RPG Title Screen Music Pack* por Irrational Machines (CC-BY 4.0).
* **Efectos de Sonido:** *Minifantasy - Dungeon SFX Pack* por Leohpaz (Itch.io).
