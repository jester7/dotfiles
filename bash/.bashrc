# Source machine-specific config if it exists
[[ -f ~/.bashrc-$HOSTNAME ]] && source ~/.bashrc-$HOSTNAME

# cloud detection and banner function
show_cloud_banner() {
    # Get terminal width
    TERM_WIDTH=$(tput cols)
    
    # detect OS or cloud provider
    if [[ "$OSTYPE" == "darwin"* ]]; then
        BG="\e[48;5;240m"        # Gray
        CLOUD="macOS"
        ICON=" " # Apple Logo
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
                ICON=" " # Azure Logo
                ;;
            *)
                BG="\e[48;5;240m"        # Generic Gray
                CLOUD="Unknown Cloud"
                ICON=" " # Generic Cloud
                ;;
        esac
    fi

    # calculate width based on text length
    TEXT="$ICON  $CLOUD - $(hostname)"

    echo ""
    printf "${BG}%*s%s%*s\e[0m\n" $(((TERM_WIDTH-${#TEXT})/2)) "" "$TEXT" $(((TERM_WIDTH-${#TEXT})/2)) ""
    echo ""
}

show_cloud_banner

# Starship config
export STARSHIP_CONFIG="$HOME/.config/starship/starship.toml"
eval "$(starship init bash)"

# Shared configurations

