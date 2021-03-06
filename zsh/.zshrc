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

setopt CLOBBER

bindkey -M "viins" "\C-R" history-incremental-search-backward

export EDITOR="vim"

# Enable keychain/ssh-agent to ask for passwords in X
export SSH_ASKPASS="/usr/lib/ssh/x11-ssh-askpass"

export GOPATH="${HOME}/src/ocean/backend"
export PATH="${HOME}/bin:${HOME}/.mix:${GOPATH}/bin:${HOME}/go/bin:${HOME}/.cabal/bin:${HOME}/.gem/ruby/2.1.0/bin:${HOME}/bin/phantomjs/bin:${HOME}/bin/heroku-client:${HOME}/bin/libexec/git-core:${PATH}:${HOME}/.gem/ruby/2.2.0/bin"
export MANPATH="${HOME}/bin/share/man:${MANPATH}"
export GOPATH=~/go

export BASE16_SHELL=$HOME/.dotfiles/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

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
# unalias topc
# unalias topm
unalias sftp
unalias rsync
unalias scp
unalias ln

alias bower='noglob bower'

unalias gcO
unalias gcf
unalias gcF
unalias gcp
unalias gcP
unalias gcr
unalias gcR
unalias gcs
unalias gcl
unalias gCl
unalias gCa
unalias gCe
unalias gCo
unalias gCO
unalias gCt
unalias gCT
unalias gd
unalias gdc
unalias gdx
unalias gdm
unalias gdu
unalias gdk
unalias gdi
unalias gf
unalias gfc
unalias gfm
unalias gfr
unalias gg
unalias ggi
unalias ggl
unalias ggL
unalias ggv
unalias ggw
unalias giA
unalias giD
unalias gmC
unalias gmF
unalias gma
unalias gmt
unalias gpa
unalias gpA
unalias gpt
unalias gpc
unalias gpp
unalias grs
unalias gR
unalias gRl
unalias gRa
unalias gRx
unalias gRm
unalias gRu
unalias gRp
unalias gRs
unalias gRb
unalias gsL
unalias gsr
unalias gS
unalias gSa
unalias gSf
unalias gSi
unalias gSI
unalias gSl
unalias gSm
unalias gSs
unalias gSu
unalias gSx

alias gf='git fetch --all --prune'

# alias wg='cd ~/src/webapp && make clean && make -j test build && make -j watch watch-test serve | ~/bin/notify-build | awk ''
#   /problem.*error.*warning/ {
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░ NOPE ░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#     print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
#   }
#
#   !/^\s+at/ { print $0 }
# '''

alias wg='cd ~/src/webapp && make clean && make -j test build && make -j watch watch-test serve | ~/bin/notify-build'

# function vim_session() {
#   if [[ -f Session.vim ]] ; then
#     /usr/bin/vim -S Session.vim $*
#   else
#     /usr/bin/vim $*
#   fi
# }
#
# alias vim=vim_session
# compdef _vim vim_session

# source /usr/share/chruby/chruby.sh

for profile in /etc/profile.d/*.sh; do
  if [ -x $profile ]; then
    . $profile
  fi
done
unset profile
export rvmsudo_secure_path=0

# Support staying in term after a default command:
#
# zsh -is eval 'make test'
#
# http://www.zsh.org/mla/users/2005/msg00599.html
if [[ $1 == eval ]]; then
  "$@"
  set --
fi
