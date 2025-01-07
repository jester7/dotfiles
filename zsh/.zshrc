# Source machine-specific config if it exists
[[ -f ~/.zshrc-$HOSTNAME ]] && source ~/.zshrc-$HOSTNAME

# cloud detection and banner function
show_cloud_banner() {
    # Get terminal width
    TERM_WIDTH=$(tput cols)
    
    # Detect OS and cloud provider
    if [[ "$OSTYPE" == "darwin"* ]]; then
        BG="\e[48;5;240m"        # Gray
        CLOUD="macOS"
        ICON=" " # Apple Logo
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

    TEXT="$ICON $CLOUD - ${HOST}"

    # Print three-line box using full terminal width
    print
    print -P "${BG}${(l:(($TERM_WIDTH-$#TEXT))/2:: :)}${TEXT}${(l:(($TERM_WIDTH-$#TEXT))/2:: :)}%f%k"
    print 
}

show_cloud_banner

export PATH="/usr/local/opt/openjdk/bin:$PATH"

export DOTNET_ROOT="$HOMEBREW_PREFIX/opt/dotnet/libexec"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

alias -g emacs="emacsclient -c -n"
alias -g ls="ls -G"
alias -g lsa="ls -lah -G"

alias -g lsagrep="ls -lah -G | grep"
alias -g psgrep="ps aux | grep"
alias -g listports="lsof -i -P -n | grep LISTEN"


# killgrep: kill a process by name
killgrep() {
  ps aux | grep $1 | awk '{print $2}' | xargs kill -9
}
export PATH="/usr/local/opt/curl/bin:$PATH:$HOME/bin"

export SUDO_EDITOR="emacsclient -c -n"

export STARSHIP_CONFIG="$HOME/.config/starship/starship.toml"
eval "$(starship init zsh)"

fastfetch

image_path="$HOME/.config/fastfetch/images/black-clover-5-leaf-clover.png"

# Check the value of $TERM_PROGRAM
# case "$TERM_PROGRAM" in
#     "iTerm.app")
#         fastfetch --iterm "$image_path"
#         ;;
#     "wezterm"|"kitty"|"ghostty")
#         fastfetch --kitty "$image_path"
#         ;;
#     *)
#        fastfetch
#         ;;
# esac
