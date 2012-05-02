# Dot Files

These configuration files set up my command line interface.

## Environment

Be aware that I use Linux; changes might be necessary for Mac OS X users.

To switch to [Zsh](http://www.zsh.org), execute:

    chsh -s /bin/zsh

Sorin stores [scripts](https://github.com/sorin-ionescu/tilde) in _~/.tilde/bin_. `PATH` and `MANPATH` must be modified in _oh-my-zsh/environment.zsh_ to match your configuration.

Some scripts and programs may have different names or extensions depending on the operating system or package manager. Check aliases in _oh-my-zsh/alias.zsh_ to fix them, if necessary.

## oh-my-zsh Theme

![sorin.oh-my-zsh theme](http://i.imgur.com/aipDQ.png "sorin.oh-my-zsh theme")

The font in the screenshot is Monaco 12 pt—the default fixed-width font on Mac OS X prior to Snow Leopard—which has been replaced by Menlo, based on DejaVu Sans Serif Mono, an inferior font. Change the font to Monaco; otherwise, the indicators described bellow will look terrible.

The colours in the image above are from the [IR_BLACK](http://blog.toddwerth.com/entries/show/6) scheme.

### Left Prompt

- oh-my-zsh — The current working directory.
- git:master — Git branch.
- ❯ — Type after this.

### Right Prompt

- ❮❮❮ — Vi command mode indicator.
- ⏎  — Non-zero return.
- ✚ — Git added.
- ⬆ - Git ahead.
- ⬇ - Git behind.
- ✖ — Git deleted.
- ✱ — Git modified.
- ➜ — Git renamed.
- ✭ - Git stashed.
- ═ — Git non-merged.
- ◼ — Git untracked.

## Installation

Clone this repository into _~/.dotfiles_, change directory into _~/.dotfiles_, and execute the `rake` install task.

    git clone git://github.com/jlamontagne/dotfiles.git ~/.dotfiles
    cd ~/.dotfiles
    rake install

Rake will **never** replace existing files but back them up into *~/.dotfiles_backup*. The dot files will be symbolically linked into the home directory. Templates will be rendered in place then symbolically linked. Since the _Rakefile_ is Mac OS X specific, it must be edited for use with key chains on other operating systems. I will welcome patches that add support for additional password managers.

## Vim Text Editor

I use Vim with [Vundle](https://github.com/gmarik/vundle), which allows for Vim plugins to be installed self-contained under their own directory in _vim/bundle_ making them easy to install and remove. The _Rakefile_ has tasks for managing Vim bundles. Read the Vundle documentation for further information.
