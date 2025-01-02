#!/bin/bash

DOTFILES="$HOME/dotfiles"
cd "$DOTFILES" || exit 1

# For .bashrc and .zshrc, rename to hostname-specific if they exist and aren't symlinks
for rc in ".bashrc" ".zshrc"; do
    if [ -f "$HOME/$rc" ] && [ ! -L "$HOME/$rc" ]; then
        echo "Moving $HOME/$rc to $HOME/$rc-$HOSTNAME"
        mv "$HOME/$rc" "$HOME/$rc-$HOSTNAME"
    fi
done

# Backup specific files/dirs, not entire directories
backup_if_exists() {
    if [ -e "$1" ]; then
        echo "Backing up $1 to $1.backup"
        mv "$1" "$1.backup"
    fi
}

# Backup only specific Emacs files, not the whole .emacs.d
backup_if_exists "$HOME/.emacs.d/init.el"
backup_if_exists "$HOME/.emacs.d/early-init.el"
if [ -d "$HOME/.emacs.d/jester" ]; then
    echo "Backing up jester directory to jester.backup"
    mv "$HOME/.emacs.d/jester" "$HOME/.emacs.d/jester.backup"
fi

# backup other config files
# backup_if_exists "$HOME/.bash_profile"
backup_if_exists "$HOME/.config/starship"
backup_if_exists "$HOME/.config/fastfetch"
backup_if_exists "$HOME/.config/ghostty"
backup_if_exists "$HOME/bin"

# Create necessary directories
mkdir -p "$HOME/.config"
mkdir -p "$HOME/bin"
mkdir -p "$HOME/.emacs.d/jester"

# Use stow to create symlinks
packages=(emacs bin zsh bash config)

for package in "${packages[@]}"; do
    echo "Stowing $package..."
    stow -v "$package"
done

echo "Dotfiles installation complete!"
