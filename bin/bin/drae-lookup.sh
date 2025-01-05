#!/bin/bash

# Get the directory where this script lives
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/diccionario_rae" && pwd )"

# Activate the virtual environment
source "$SCRIPT_DIR/.venv/bin/activate"

# Run the Python script with all arguments passed to this script
python "$SCRIPT_DIR/buscar_en_drae.py" "$@"

# Deactivate the virtual environment
deactivate
