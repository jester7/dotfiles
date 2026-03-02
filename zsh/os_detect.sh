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

# Cloud provider detection for starship prompt and banner
if [[ "$OSTYPE" == "darwin"* ]]; then
    export CLOUD_BG="\e[48;5;240m"        # Gray
    export CLOUD_NAME="macOS"
    export CLOUD_ICON="" # Apple Logo
else
    local dmi_info=""
    [[ -f /sys/class/dmi/id/product_name ]] && dmi_info=$(cat /sys/class/dmi/id/product_name)
    [[ -f /sys/class/dmi/id/sys_vendor ]] && dmi_info="$dmi_info $(cat /sys/class/dmi/id/sys_vendor)"

    case "$dmi_info" in
        *"Google"*)
            export CLOUD_BG="\e[48;2;66;133;244m" # Google Blue
            export CLOUD_NAME="Google Cloud"
            export CLOUD_ICON="󱇶" # GCP Logo
            ;;
        *"Amazon"*)
            export CLOUD_BG="\e[48;2;255;153;0m"  # AWS Orange
            export CLOUD_NAME="AWS"
            export CLOUD_ICON="" # AWS Logo
            ;;
        *"Microsoft"*)
            export CLOUD_BG="\e[48;2;0;120;212m"  # Azure Blue
            export CLOUD_NAME="Azure"
            export CLOUD_ICON="" # Azure Logo
            ;;
        *)
            export CLOUD_BG="\e[48;5;240m"        # Generic Gray
            export CLOUD_NAME="Linux"
            export CLOUD_ICON="󰅟" # Generic Cloud
            ;;
    esac
fi

# cloud detection and banner function
show_cloud_banner() {
    TERM_WIDTH=$(tput cols)

    local hostname_str="${HOST:-$(hostname)}"
    local text="$CLOUD_ICON $CLOUD_NAME - $hostname_str"
    local padding=$(( (TERM_WIDTH - ${#text}) / 2 ))

    echo ""
    printf "${CLOUD_BG}%*s%s%*s\e[0m\n" "$padding" "" "$text" "$padding" ""
    echo ""
}
