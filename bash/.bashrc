# Source os detection flags script
source "$HOME/os_detect.sh"

# Source machine-specific config if it exists
[[ -f ~/.bashrc-$HOSTNAME ]] && source ~/.bashrc-$HOSTNAME

if [[ $- == *i* ]]; then

    show_cloud_banner

    # Starship config
    export STARSHIP_CONFIG="$HOME/.config/starship/starship.toml"
    eval "$(starship init bash)"

    # fastfetch info
    image_path="$HOME/.config/fastfetch/images/black-clover-5-leaf-clover.png"
    fastfetch --kitty "$image_path"

    if [[ "$IS_LINUX" == "true" ]]; then
        # Run system status check for linux servers
        if [[ -x "$HOME/bin/system-status.sh" ]]; then
            "$HOME/bin/system-status.sh" -p -s -m
        fi
    fi
fi
