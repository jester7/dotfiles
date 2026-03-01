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

# cloud detection and banner function
show_cloud_banner() {
    TERM_WIDTH=$(tput cols)

    if [[ "$OSTYPE" == "darwin"* ]]; then
        BG="\e[48;5;240m"        # Gray
        CLOUD="macOS"
        ICON=" " # Apple Logo
    elif [[ -f /sys/class/dmi/id/product_name ]]; then
        case $(cat /sys/class/dmi/id/product_name) in
            *"Google"*)
                BG="\e[48;2;66;133;244m" # Google Blue
                CLOUD="Google Cloud"
                ICON="󱇶 " # GCP Logo
                ;;
            *"Amazon"*)
                BG="\e[48;2;255;153;0m"  # AWS Orange
                CLOUD="AWS"
                ICON=" " # AWS Logo
                ;;
            *"Microsoft"*)
                BG="\e[48;2;0;120;212m"  # Azure Blue
                CLOUD="Azure"
                ICON=" " # Azure Logo
                ;;
            *)
                BG="\e[48;5;240m"        # Generic Gray
                CLOUD="Unknown Cloud"
                ICON=" " # Generic Cloud
                ;;
        esac
    fi

    local hostname_str="${HOST:-$(hostname)}"
    local text="$ICON $CLOUD - $hostname_str"
    local padding=$(( (TERM_WIDTH - ${#text}) / 2 ))

    echo ""
    printf "${BG}%*s%s%*s\e[0m\n" "$padding" "" "$text" "$padding" ""
    echo ""
}
