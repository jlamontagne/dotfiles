# https://github.com/paulirish/dotfiles/blob/master/fish/config.fish

source ~/.config/fish/aliases.fish
source ~/.config/fish/functions.fish

nvm use default

if status --is-login
  set PATH $PATH ~/bin
  set PATH $PATH ~/bin/heroku-client
  set PATH $PATH ~/bin/phantomjs-bin
  set -x NVM_DIR ~/.nvm
end

if status --is-interactive
  set -l IFS
  eval (keychain --eval --quiet -Q id_rsa)
end

# Git prompt status
set __fish_git_prompt_show_informative_status 'no'

# Local prompt customization
set -e fish_greeting

set -q ____abbrs_set
if test $status -ne 0
  echo "Setting default abbreviations"
  set -U ____abbrs_set 1

  # abbr -a gco git checkout

  # Branch (b)
  abbr -a gb git branch
  abbr -a gbc git checkout -b
  abbr -a gbl git branch -v
  abbr -a gbL git branch -av
  abbr -a gbx git branch -d
  abbr -a gbX git branch -D
  abbr -a gbm git branch -m
  abbr -a gbM git branch -M
  abbr -a gbs git show-branch
  abbr -a gbS git show-branch -a

  # Commit (c)
  abbr -a gc git commit --verbose
  abbr -a gca git commit --verbose --all
  abbr -a gcm git commit --message
  abbr -a gco git checkout
  abbr -a gcO git checkout --patch
  abbr -a gcp git cherry-pick --ff
  abbr -a gcP git cherry-pick --no-commit
  abbr -a gcR git reset "HEAD^"

  # Data (d)
  abbr -a gd git ls-files
  abbr -a gdc git ls-files --cached
  abbr -a gdx git ls-files --deleted
  abbr -a gdm git ls-files --modified
  abbr -a gdu git ls-files --other --exclude-standard
  abbr -a gdk git ls-files --killed
  abbr -a gdi git status --porcelain --short --ignored | sed -n "s/^!! //p"

  # Fetch (f)
  abbr -a gf git fetch --all --prune
  abbr -a gfm git pull
  abbr -a gfr git pull --rebase

  # Index (i)
  abbr -a gia git add
  abbr -a giA git add --patch
  abbr -a giu git add --update
  abbr -a gid git diff --no-ext-diff --cached
  abbr -a giD git diff --no-ext-diff --cached --word-diff
  abbr -a gir git reset
  abbr -a giR git reset --patch
  abbr -a gix git rm -r --cached
  abbr -a giX git rm -rf --cached

  # Log (l)
  abbr -a gl git log --color --graph --pretty=\"format:%Cred%h%Creset -%C\(yellow\)%d%Creset \%s \%Cgreen\(%cr\) \%C\(bold blue\)\<%an\>%Creset\" --abbrev-commit

  # Merge (m)
  abbr -a gm git merge
  abbr -a gmC git merge --no-commit
  abbr -a gmF git merge --no-ff
  abbr -a gma git merge --abort
  abbr -a gmt git mergetool

  # Push (p)
  abbr -a gp git push
  abbr -a gpf git push --force
  abbr -a gpa git push --all
  abbr -a gpA git push --all; and git push --tags
  abbr -a gpt git push --tags

  # Rebase (r)
  abbr -a gr git rebase
  abbr -a gra git rebase --abort
  abbr -a grc git rebase --continue
  abbr -a gri git rebase --interactive
  abbr -a grs git rebase --skip

  # Stash (s)
  abbr -a gs git stash
  abbr -a gsa git stash apply
  abbr -a gsx git stash drop
  abbr -a gsX git-stash-clear-interactive
  abbr -a gsl git stash list
  abbr -a gsd git stash show --patch --stat
  abbr -a gsp git stash pop
  abbr -a gss git stash save --include-untracked
  abbr -a gsS git stash save --patch --no-keep-index
  abbr -a gsw git stash save --include-untracked --keep-index

  # Working Copy (w)
  abbr -a gws git status --short
  abbr -a gwS git status
  abbr -a gwd git diff --no-ext-diff
  abbr -a gwD git diff --no-ext-diff --word-diff
  abbr -a gwr git reset --soft
  abbr -a gwR git reset --hard
  abbr -a gwc git clean -n
  abbr -a gwC git clean -f
  abbr -a gwx git rm -r
  abbr -a gwX git rm -rf
end
