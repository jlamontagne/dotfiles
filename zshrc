#
# Sets Oh My Zsh options.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Set the path to Oh My Zsh.
export OMZ="$HOME/.oh-my-zsh"

# Set the key mapping style to 'emacs' or 'vi'.
zstyle ':omz:module:editor' keymap 'emacs'

# Auto convert .... to ../..
zstyle ':omz:module:editor' dot-expansion 'no'

# Set case-sensitivity for completion, history lookup, etc.
zstyle ':omz:*:*' case-sensitive 'no'

# Color output (auto set to 'no' on dumb terminals).
zstyle ':omz:*:*' color 'yes'

# Auto set the tab and window titles.
zstyle ':omz:module:terminal' auto-title 'yes'

# Set the Zsh modules to load (man zshmodules).
# zstyle ':omz:load' zmodule 'attr' 'stat'

# Set the Zsh functions to load (man zshcontrib).
# zstyle ':omz:load' zfunction 'zargs' 'zmv'

# Set the Oh My Zsh modules to load (browse modules).
zstyle ':omz:load' omodule \
  'alias' \
  'completion' \
  'directory' \
  'editor' \
  'environment' \
  'git' \
  'history' \
  'history-substring-search' \
  'node' \
  'prompt' \
  'rsync' \
  'ruby' \
  'terminal' \
  'utility'

# Set the prompt theme to load.
# Setting it to 'random' loads a random theme.
# Auto set to 'off' on dumb terminals.
zstyle ':omz:module:prompt' theme 'sorin'

# This will make you shout: OH MY ZSHELL!
source "$OMZ/init.zsh"

# Customize to your needs...

# Enable keychain/ssh-agent to ask for passwords in X
export SSH_ASKPASS="/usr/bin/x11-ssh-askpass"
