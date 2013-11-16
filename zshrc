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

bindkey -M "viins" "\C-R" history-incremental-search-backward

# Enable keychain/ssh-agent to ask for passwords in X
export SSH_ASKPASS="/usr/bin/x11-ssh-askpass"

export PATH="${HOME}/bin:${HOME}/bin/phantomjs/bin:${HOME}/bin/heroku-client:${HOME}/bin/libexec/git-core:${PATH}"
export MANPATH="${HOME}/bin/share/man:${MANPATH}"

# Disable Ctrl-S/Q flow control
#stty -ixon

# Disable unwanted aliases
unalias sl
unalias mv
unalias rm
unalias heroku
unalias run-help
unalias http-serve
unalias df
unalias du
unalias ack
unalias cd
unalias cp
unalias d
unalias e
unalias ebuild
unalias ftp
unalias find
unalias type
unalias rake
unalias p
unalias o
unalias man
unalias mkdir
unalias mysql
unalias which-command
unalias topc
unalias topm
unalias sftp
unalias rsync
unalias scp
unalias ln
