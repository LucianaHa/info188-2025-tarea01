# ==========================================
# Makefile para HaskiRPG
# ==========================================

# Nombre del proyecto (opcional, solo visual)
PROJECT_NAME = haskirpg

# .PHONY indica que estos no son archivos reales, sino comandos
.PHONY: all build run clean repl update help

# Comando por defecto (si solo escribes 'make')
all: build

# ------------------------------------------
# Comandos Principales
# ------------------------------------------

# Compila el proyecto sin ejecutarlo
build:
	@echo "Compilando $(PROJECT_NAME)..."
	cabal build

# Compila y ejecuta el juego
run:
	@echo "Iniciando $(PROJECT_NAME)..."
	cabal run

# Limpia los archivos generados (útil si algo se rompe)
clean:
	@echo "Limpiando archivos temporales..."
	cabal clean

# Abre la consola interactiva de Haskell (GHCi) con tus librerías cargadas
repl:
	@echo "Abriendo consola interactiva..."
	cabal repl

# Actualiza las dependencias de Cabal
update:
	@echo "Actualizando lista de paquetes..."
	cabal update

# ------------------------------------------
# Comandos Especiales (WSL / Audio)
# ------------------------------------------

# Usa este si el audio falla en WSL (Linux en Windows)
run-wsl:
	@echo "Ejecutando modo compatible WSL (PulseAudio)..."
	SDL_AUDIODRIVER=pulseaudio cabal run

# Ayuda: Muestra los comandos disponibles
help:
	@echo "Comandos disponibles:"
	@echo "  make run      -> Jugar (Compila y ejecuta)"
	@echo "  make build    -> Solo compilar"
	@echo "  make clean    -> Borrar compilaciones previas (arregla errores raros)"
	@echo "  make repl     -> Abrir consola de pruebas GHCi"
	@echo "  make run-wsl  -> Usar si no tienes sonido en WSL"