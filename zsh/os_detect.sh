#!/bin/bash

# OS and Architecture Detection
OS=$(uname -s)
ARCH=$(uname -m)

# Flag Variables
export IS_MACOS=false
export IS_MACOS_X86_64=false
export IS_MACOS_ARM64=false
export IS_LINUX=false
export IS_LINUX_ARM64=false
export IS_LINUX_X86_64=false

if [[ "$OS" == "Darwin" ]]; then
  export IS_MACOS=true
  if [[ "$ARCH" == "x86_64" ]]; then
    export IS_MACOS_X86_64=true
  elif [[ "$ARCH" == "arm64" ]]; then
    export IS_MACOS_ARM64=true
  fi
elif [[ "$OS" == "Linux" ]]; then
  export IS_LINUX=true
  if [[ "$ARCH" == "arm64" ]]; then
    export IS_LINUX_ARM64=true
  elif [[ "$ARCH" == "x86_64" ]]; then
    export IS_LINUX_X86_64=true
  fi
fi
