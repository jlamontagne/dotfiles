alias c='pygmentize -O style=monokai -f console256 -g'

# Git
alias g='git'

# Branch (b)
alias gb='git branch'
alias gbc='git checkout -b'
alias gbl='git branch -v'
alias gbL='git branch -av'
alias gbx='git branch -d'
alias gbX='git branch -D'
alias gbm='git branch -m'
alias gbM='git branch -M'
alias gbs='git show-branch'
alias gbS='git show-branch -a'

# Commit (c)
alias gc='git commit --verbose'
alias gca='git commit --verbose --all'
alias gcm='git commit --message'
alias gco='git checkout'
alias gcO='git checkout --patch'
alias gcp='git cherry-pick --ff'
alias gcP='git cherry-pick --no-commit'
alias gcR='git reset "HEAD^"'

# Data (d)
alias gd='git ls-files'
alias gdc='git ls-files --cached'
alias gdx='git ls-files --deleted'
alias gdm='git ls-files --modified'
alias gdu='git ls-files --other --exclude-standard'
alias gdk='git ls-files --killed'
alias gdi='git status --porcelain --short --ignored | sed -n "s/^!! //p"'

# Fetch (f)
alias gf='git fetch --all --prune'
alias gfm='git pull'
alias gfr='git pull --rebase'

# Index (i)
alias gia='git add'
alias giA='git add --patch'
alias giu='git add --update'
alias gid='git diff --no-ext-diff --cached'
alias giD='git diff --no-ext-diff --cached --word-diff'
alias gir='git reset'
alias giR='git reset --patch'
alias gix='git rm -r --cached'
alias giX='git rm -rf --cached'

# Log (l)
alias gl='git log --color --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit'
#alias gl='git log --topo-order --pretty=format:"${_git_log_medium_format}"'
#alias gls='git log --topo-order --stat --pretty=format:"${_git_log_medium_format}"'
#alias gld='git log --topo-order --stat --patch --full-diff --pretty=format:"${_git_log_medium_format}"'
#alias glo='git log --topo-order --pretty=format:"${_git_log_oneline_format}"'
#alias glg='git log --topo-order --all --graph --pretty=format:"${_git_log_oneline_format}"'
#alias glb='git log --topo-order --pretty=format:"${_git_log_brief_format}"'
#alias glc='git shortlog --summary --numbered'

# Merge (m)
alias gm='git merge'
alias gmC='git merge --no-commit'
alias gmF='git merge --no-ff'
alias gma='git merge --abort'
alias gmt='git mergetool'

# Push (p)
alias gp='git push'
alias gpf='git push --force'
alias gpa='git push --all'
alias gpA='git push --all; and git push --tags'
alias gpt='git push --tags'

# Rebase (r)
alias gr='git rebase'
alias gra='git rebase --abort'
alias grc='git rebase --continue'
alias gri='git rebase --interactive'
alias grs='git rebase --skip'

# Stash (s)
alias gs='git stash'
alias gsa='git stash apply'
alias gsx='git stash drop'
alias gsX='git-stash-clear-interactive'
alias gsl='git stash list'
alias gsd='git stash show --patch --stat'
alias gsp='git stash pop'
alias gss='git stash save --include-untracked'
alias gsS='git stash save --patch --no-keep-index'
alias gsw='git stash save --include-untracked --keep-index'

# Working Copy (w)
alias gws='git status --short'
alias gwS='git status'
alias gwd='git diff --no-ext-diff'
alias gwD='git diff --no-ext-diff --word-diff'
alias gwr='git reset --soft'
alias gwR='git reset --hard'
alias gwc='git clean -n'
alias gwC='git clean -f'
alias gwx='git rm -r'
alias gwX='git rm -rf'

alias wg='cd ~/src/webapp; and make clean; and make -j test build; and make -j watch watch-test serve | awk ''
  /problem.*error.*warning/ {
    print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
    print "░░░░░░░░░░░░░▄▄▄▄▄▄▄░░░░░░░░░";
    print "░░░░░░░░░▄▀▀▀░░░░░░░▀▄░░░░░░░";
    print "░░░░░░░▄▀░░░░░░░░░░░░▀▄░░░░░░";
    print "░░░░░░▄▀░░░░░░░░░░▄▀▀▄▀▄░░░░░";
    print "░░░░▄▀░░░░░░░░░░▄▀░░██▄▀▄░░░░";
    print "░░░▄▀░░▄▀▀▀▄░░░░█░░░▀▀░█▀▄░░░";
    print "░░░█░░█▄▄░░░█░░░▀▄░░░░░▐░█░░░";
    print "░░▐▌░░█▀▀░░▄▀░░░░░▀▄▄▄▄▀░░█░░";
    print "░░▐▌░░█░░░▄▀░░░░░░░░░░░░░░█░░";
    print "░░▐▌░░░▀▀▀░░░░░░░░░░░░░░░░▐▌░";
    print "░░▐▌░░░░░░░░░░░░░░░▄░░░░░░▐▌░";
    print "░░▐▌░░░░░░░░░▄░░░░░█░░░░░░▐▌░";
    print "░░░█░░░░░░░░░▀█▄░░▄█░░░░░░▐▌░";
    print "░░░▐▌░░░░░░░░░░▀▀▀▀░░░░░░░▐▌░";
    print "░░░░█░░░░░░░░░░░░░░░░░░░░░█░░";
    print "░░░░▐▌▀▄░░░░░░░░░░░░░░░░░▐▌░░";
    print "░░░░░█░░▀░░░░░░░░░░░░░░░░▀░░░";
    print "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░";
  }

  !/^\s+at/ { print $0 }
'''
