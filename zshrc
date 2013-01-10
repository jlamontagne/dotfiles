#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# Enable keychain/ssh-agent to ask for passwords in X
export SSH_ASKPASS="/usr/bin/x11-ssh-askpass"

export PATH="${HOME}/bin:${HOME}/bin/heroku-client:${HOME}/bin/libexec/git-core:${PATH}"
export MANPATH="${HOME}/bin/share/man:${MANPATH}"
