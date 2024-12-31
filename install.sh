#!/bin/bash

DOTFILES="$HOME/dotfiles"
cd "$DOTFILES" || exit 1

# Backup existing configs
backup_if_exists() {
    if [ -e "$1" ]; then
        echo "Backing up $1 to $1.backup"
        mv "$1" "$1.backup"
    fi
}

# Backup existing configurations
backup_if_exists "$HOME/.emacs.d"
backup_if_exists "$HOME/.zshrc"
backup_if_exists "$HOME/.zshrc-$(hostname)"
backup_if_exists "$HOME/.bashrc"
backup_if_exists "$HOME/.bash_profile"
backup_if_exists "$HOME/.config/starship"
backup_if_exists "$HOME/.config/fastfetch"
backup_if_exists "$HOME/.config/ghostty"
backup_if_exists "$HOME/bin"

# Create necessary directories
mkdir -p "$HOME/.config"
mkdir -p "$HOME/bin"

# Use stow to create symlinks
packages=(emacs bin zsh bash config)

for package in "${packages[@]}"; do
    echo "Stowing $package..."
    stow -v "$package"
done

echo "Dotfiles installation complete!"

