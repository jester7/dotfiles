# Dotfiles

My personal dotfiles, managed with GNU Stow.

## Installation

1. Clone this repository:
   #+end_srcbash
   git clone https://github.com/yourusername/dotfiles.git ~/dotfiles
   #+begin_src 

2. Install GNU Stow:
   #+end_srcbash
   brew install stow
   #+begin_src 

3. Use Stow to symlink configurations:
   #+end_srcbash
   cd ~/dotfiles
   stow emacs bin zsh bash config
