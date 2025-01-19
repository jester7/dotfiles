#!/bin/bash

# Get the directory where this script lives
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/buscar_dle" && pwd )"

# Activate the virtual environment
source "$SCRIPT_DIR/.venv/bin/activate"

# Activate the virtual environment and run the Python script
(
  source "$SCRIPT_DIR/.venv/bin/activate"
  python "$SCRIPT_DIR/buscar_en_diccionario.py" "$@"
)
