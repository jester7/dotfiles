# cloud detection and banner function
show_cloud_banner() {
    # detect OS or cloud provider
    if [[ "$OSTYPE" == "darwin"* ]]; then
        BG="\e[48;5;240m"        # Gray
        CLOUD="macOS"
        ICON="" # Apple Logo
    elif [ -f /sys/class/dmi/id/product_name ]; then
        case $(cat /sys/class/dmi/id/product_name) in
            *"Google"*)
                BG="\e[48;2;66;133;244m" # Google Blue
                CLOUD="Google Cloud"
                ICON="󱇶 " # GCP Logo
                ;;
            *"Amazon"*)
                BG="\e[48;2;255;153;0m"  # AWS Orange
                CLOUD="AWS"
                ICON=" " # AWS Logo
                ;;
            *"Microsoft"*)
                BG="\e[48;2;0;120;212m"  # Azure Blue
                CLOUD="Azure"
                ICON="󰠅" # Azure Logo
                ;;
            *)
                BG="\e[48;5;240m"        # Generic Gray
                CLOUD="Unknown Cloud"
                ICON="󰒋" # Generic Cloud
                ;;
        esac
    fi

    # calculate width based on text length
    TEXT="   $ICON  $CLOUD - $(hostname)   "
    WIDTH=${#TEXT}

    # print three-line box
    echo -e "${BG}$(printf ' %.0s' $(seq 1 $WIDTH))\e[0m"
    echo -e "${BG}${TEXT}\e[0m"
    echo -e "${BG}$(printf ' %.0s' $(seq 1 $WIDTH))\e[0m"
}

show_cloud_banner



# Source machine-specific config
source ~/.bashrc-$HOSTNAME

# Starship config
export STARSHIP_CONFIG="$HOME/.config/starship/starship.toml"
eval "$(starship init bash)"

# Shared configurations

