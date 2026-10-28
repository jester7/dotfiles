# Source os detection flags script
source "$HOME/os_detect.sh"

# Source machine-specific config if it exists. zsh on macOS sets $HOST but not
# $HOSTNAME (bash, and zsh on the Linux boxes, set HOSTNAME), so fall back to it.
[[ -f ~/.zshrc-${HOSTNAME:-$HOST} ]] && source ~/.zshrc-${HOSTNAME:-$HOST}

if [[ "$IS_MACOS" == "true" ]]; then
    export PATH="/usr/local/opt/openjdk/bin:$PATH"
    export DOTNET_ROOT="$HOMEBREW_PREFIX/opt/dotnet/libexec"
fi

export NVM_DIR="$HOME/.nvm"

# Lazy-load nvm so Homebrew Node stays first on PATH until explicitly needed.
load_nvm() {
    unset -f nvm load_nvm
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
}

nvm() {
    load_nvm
    nvm "$@"
}

normalize_path() {
    local dir
    local -a raw_path normalized_path

    raw_path=("${(@s/:/)PATH}")
    for dir in "${raw_path[@]}"; do
        [[ -z "$dir" ]] && continue
        [[ "$dir" == "~/"* ]] && dir="$HOME/${dir#\~/}"
        normalized_path+=("$dir")
    done

    typeset -U normalized_path
    path=("${normalized_path[@]}")
    export PATH
}

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
# OpenCode 2 (installed with the official installer's --no-modify-path)
[[ -d "$HOME/.opencode/bin" ]] && export PATH="$HOME/.opencode/bin:$PATH"
if [[ "$IS_MACOS" == "true" ]]; then
    export PATH="/usr/local/opt/curl/bin:$PATH:$HOMEBREW_PREFIX/opt/postgresql@18/bin"
fi

# MacPorts: sit right after Homebrew's /usr/local/bin so Apple's /usr/bin stubs
# don't shadow it, while Homebrew still wins a name collision until the brew
# copy is uninstalled. Dropping any existing entries first keeps this order even
# if the MacPorts installer's PATH line in ~/.zprofile put /opt/local first.
if [[ "$IS_MACOS" == "true" && -d /opt/local/bin ]]; then
    path=("${(@)path:#/opt/local/(bin|sbin)}")
    if (( ${path[(Ie)/usr/local/bin]} )); then
        path[${path[(Ie)/usr/local/bin]}+1,0]=(/opt/local/bin /opt/local/sbin)
    else
        path+=(/opt/local/bin /opt/local/sbin)
    fi
fi

normalize_path

if [[ "$IS_MACOS" == "true" ]]; then
    export SUDO_EDITOR="emacsclient -c -n"
fi

if [[ -o interactive ]]; then
    show_cloud_banner

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

    if [[ "$IS_LINUX" == "true" ]]; then
        # Run system status check for linux servers
        if [[ -x "$HOME/bin/system-status.sh" ]]; then
            "$HOME/bin/system-status.sh" -p -s -m
        fi
    fi
fi

# Unity CLI
[[ -f "$HOME/.unity/env" ]] && . "$HOME/.unity/env"

# Blocks the pnpm and Docker Desktop installers append to ~/.zshrc, kept here so
# they survive re-stowing. Guarded on the install actually existing (not just on
# macOS) so machines without these tools skip them, including the compinit call.
if [[ -d "$HOME/Library/pnpm" ]]; then
    # pnpm
    export PNPM_HOME="$HOME/Library/pnpm"
    case ":$PATH:" in
      *":$PNPM_HOME/bin:"*) ;;
      *) export PATH="$PNPM_HOME/bin:$PATH" ;;
    esac
    # pnpm end
fi
if [[ -d "$HOME/.docker/completions" ]]; then
    # The following lines have been added by Docker Desktop to enable Docker CLI completions.
    fpath=("$HOME/.docker/completions" $fpath)
    autoload -Uz compinit
    (( ${+_comps[docker]} )) || compinit
    # End of Docker CLI completions
fi
