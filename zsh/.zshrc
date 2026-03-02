# Source os detection flags script
source "$HOME/os_detect.sh"

# Source machine-specific config if it exists
[[ -f ~/.zshrc-$HOSTNAME ]] && source ~/.zshrc-$HOSTNAME

if [[ "$IS_MACOS" == "true" ]]; then
    export PATH="/usr/local/opt/openjdk/bin:$PATH"
    export DOTNET_ROOT="$HOMEBREW_PREFIX/opt/dotnet/libexec"
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

if [[ "$IS_MACOS" == "true" ]]; then
    alias emacs="emacsclient -c -n"
fi
alias ls="ls -G"
alias lsa="ls -lah -G"

alias lsagrep="ls -lah -G | grep"
alias psgrep="ps aux | grep"
alias listports="lsof -i -P -n | grep LISTEN"


# killgrep: kill a process by name
killgrep() {
  pkill -9 "$1"
}
export PATH="$HOME/.local/bin:$PATH:$HOME/bin"
if [[ "$IS_MACOS" == "true" ]]; then
    export PATH="/usr/local/opt/curl/bin:$PATH:$HOMEBREW_PREFIX/opt/postgresql@18/bin"
fi

if [[ "$IS_MACOS" == "true" ]]; then
    export SUDO_EDITOR="emacsclient -c -n"
fi

if [[ -o interactive ]]; then
    show_cloud_banner

    if [[ "$IS_LINUX" == "true" ]]; then
        # Run system status check for linux servers
        if [[ -x "$HOME/bin/system-status.sh" ]]; then
            "$HOME/bin/system-status.sh" -m -s
        fi
    fi

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
fi
