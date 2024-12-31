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

image_path="$HOME/.config/fastfetch/images/black-clover-5-leaf-clover.png"

# Check the value of $TERM_PROGRAM
case "$TERM_PROGRAM" in
    "iTerm.app")
        fastfetch --iterm "$image_path"
        ;;
    "wezterm"|"kitty"|"ghostty")
        fastfetch --kitty "$image_path"
        ;;
    *)
       fastfetch
        ;;
esac
