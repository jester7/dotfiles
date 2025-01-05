# jester7 Dotfiles

My personal dotfiles managed with GNU Stow. This repository contains configurations for:

- Emacs (with separate private configurations)
- Shell environments (zsh for macOS, bash for Linux)
- Utility scripts
- Terminal configurations (Ghostty, Starship prompt, Fastfetch)

## Structure
``` bash
.
├── bash/                   # Bash configuration
│   └── .bashrc             # Main bash config
├── bin/                    # Utility scripts
│   └── bin/               
│       ├── *.bb            # Babashka scripts
│       ├── *.py            # Python scripts
│       └── *.sh            # Shell scripts
│
├── config/                 # Application configs
│   └── .config/
│       ├── fastfetch/      # System info display
│       ├── ghostty/        # Terminal emulator
│       └── starship/       # Shell prompt
│
├── emacs/                  # Emacs configuration
│   └── .emacs.d/
│       ├── init.el         # Main init file
│       ├── early-init.el   # Early initialization
│       └── jester/         # Custom configurations
│           ├── *.el        # Public configs
│           └── private/    # Private configs (submodule)
│
├── zsh/                    # Zsh configuration
│   └── .zshrc              # Main zsh config
├── install.sh              # Installation script
└── manage-dotfiles.sh      # Management script
```

## Installation

### Full Setup (macOS)
``` bash
# Clone repository with submodules
git clone --recursive git@github.com:jester7/dotfiles.git ~/dotfiles

# Install GNU Stow
brew install stow

# Run installation script

cd ~/dotfiles
./install.sh
```

### Minimal Setup (Linux Servers)

#### Clone without private configs
``` bash
git clone git@github.com:jester7/dotfiles.git ~/dotfiles

# Install GNU Stow (Debian/Ubuntu)
sudo apt-get install stow

# Install specific configurations
cd ~/dotfiles
stow emacs bash config/starship
 
```


## Management

The `manage-dotfiles.sh` script provides various operations:

Usage: manage-dotfiles.sh [options]

Options:
    -h, --help      Show help message
    -p, --push      Commit and push changes
    -l, --pull      Pull latest changes
    -s, --submod    Update private submodule
    -r, --restow    Restow all packages (default)
    -a, --all       Do everything

Common usage:
    ./manage-dotfiles.sh          # Just restow packages
    ./manage-dotfiles.sh -lr      # Pull and restow
    ./manage-dotfiles.sh -lrs     # Pull, update private configs, and restow

## Components

### Emacs
- Complete Emacs configuration with early-init.el and init.el
- Custom elisp functions in jester/
- Private configurations (mu4e, org-roam) in separate repository

### Shell
- Zsh configuration for macOS
- Bash configuration for Linux servers
- Machine-specific configurations via hostname detection

### Utility Scripts
- Babashka scripts for Clojure utilities
- Python scripts for various tools
- Shell scripts for system maintenance

### Terminal
- Starship prompt configuration
- Ghostty terminal settings
- Fastfetch system information display

## Private Configurations

Some configurations containing sensitive or machine-specific settings are stored in a separate private repository, managed as a git submodule in `emacs/.emacs.d/jester/private/`.
