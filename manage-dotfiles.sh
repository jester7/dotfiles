#!/bin/bash

print_usage() {
    cat << EOF
Usage: $(basename "$0") [options]

Manage dotfiles repository and symlinks

Options:
    -h, --help      Show this help message
    -p, --push      Commit and push changes in main repo
    -l, --pull      Pull latest changes from remote
    -s, --submod    Update private submodule (commit/push if changed)
    -r, --restow    Restow all packages (default if no options given)
    -a, --all       Do everything (pull, submod, push, restow)

Examples:
    $(basename "$0")         # Just restow packages
    $(basename "$0") -p      # Commit and push changes
    $(basename "$0") -ls     # Pull and update submodule
    $(basename "$0") -a      # Do everything
EOF
}

# Must be run from dotfiles directory
if [ ! -d ".git" ]; then
    echo "Error: Must be run from dotfiles directory"
    exit 1
fi

do_pull=false
do_push=false
do_submod=false
do_restow=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            print_usage
            exit 0
            ;;
        -p|--push)
            do_push=true
            shift
            ;;
        -l|--pull)
            do_pull=true
            shift
            ;;
        -s|--submod)
            do_submod=true
            shift
            ;;
        -r|--restow)
            do_restow=true
            shift
            ;;
        -a|--all)
            do_pull=true
            do_push=true
            do_submod=true
            do_restow=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            print_usage
            exit 1
            ;;
    esac
done

# If no options given, just restow
if [ "$do_pull" = false ] && [ "$do_push" = false ] && [ "$do_submod" = false ] && [ "$do_restow" = false ]; then
    do_restow=true
fi

# Pull if requested
if [ "$do_pull" = true ]; then
    echo "Pulling latest changes..."
    git pull
fi

# Update submodule if requested
if [ "$do_submod" = true ]; then
    echo "Checking private repo..."
    cd emacs/.emacs.d/jester/private || exit 1
    if [ -n "$(git status --porcelain)" ]; then
        echo "Changes detected in private repo:"
        git status --short
        read -p "Commit and push private changes? (y/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            git add .
            read -p "Enter commit message: " commit_msg
            git commit -m "$commit_msg"
            git push
            cd ~/dotfiles || exit 1
            git add emacs/.emacs.d/jester/private
            git commit -m "Update private submodule reference"
        fi
    else
        echo "No changes in private repo"
    fi
    cd ~/dotfiles || exit 1
fi

# Push if requested
if [ "$do_push" = true ]; then
    echo "Checking for changes in main repo..."
    if [ -n "$(git status --porcelain)" ]; then
        echo "Changes detected in main repo:"
        git status --short
        read -p "Commit and push changes? (y/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            git add .
            read -p "Enter commit message: " commit_msg
            git commit -m "$commit_msg"
            git push
        fi
    else
        echo "No changes in main repo"
    fi
fi

# Restow if requested
if [ "$do_restow" = true ]; then
    echo "Restowing all packages..."
    stow -R emacs bin zsh bash config
fi

echo "Done!"
