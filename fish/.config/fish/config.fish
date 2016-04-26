# https://github.com/paulirish/dotfiles/blob/master/fish/config.fish

source ~/.config/fish/aliases.fish
source ~/.config/fish/functions.fish

if status --is-login
  set PATH $PATH ~/bin
  set PATH $PATH ~/bin//heroku-client
  set PATH $PATH ~/bin/phantomjs-bin
end
