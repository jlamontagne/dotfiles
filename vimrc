" ----------------------------------------------------------------------------
"          FILE: .vimrc
"   DESCRIPTION: Vim configuration file
"        AUTHOR: Sorin Ionescu <sorin.ionescu@gmail.com>
"       VERSION: 1.3.13
" ----------------------------------------------------------------------------

if v:version < 703
    echo '.vimrc requires Vim 7.3 or greater'
    finish
endif

" Bundles ----------------------------------------------------------------- {{{

    " Vundle {{{

        " Turn off vi compatibility.
        set nocompatible
        " Turning filetype off is necessary to load ftdetect files.
        filetype off
        " Install Vundle for bundles to work:
        "     git clone http://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
        set rtp+=~/.vim/bundle/vundle/
        call vundle#rc()

    " }}}
    " General {{{

        Bundle 'gmarik/vundle'
        " Identifies file indentation.
        Bundle 'Raimondi/YAIFA'
        Bundle 'godlygeek/tabular'
        Bundle 'sjl/gundo.vim'
        Bundle 'tpope/vim-repeat'
        " Provides pairs of bracket mappings for buffer, file navigation and editing.
        Bundle 'tpope/vim-unimpaired'
        Bundle 'tpope/vim-scriptease'
        " Renames the file in the current buffer.
        Bundle 'vim-scripts/Rename2'
        " Kills a  buffer without closing the split.
        Bundle 'vim-scripts/bufkill.vim'
        " Show marks in a gutter.
        Bundle 'vim-scripts/ShowMarks7'
        Bundle 'Lokaltog/vim-easymotion'
        " History of yanks, changes, and deletes.
        " Bundle 'vim-scripts/YankRing.vim'
        Bundle 'scrooloose/nerdtree'
        Bundle 'kien/ctrlp.vim'
        " Easily search for, substitute, and abbreviate multiple variants of a word.
        Bundle 'tpope/vim-abolish'
        " Automatically restore one file's cursor position and folding information
        " after restart vim.
        " Bundle 'vim-scripts/restore_view.vim'
        Bundle 'coderifous/textobj-word-column.vim'
        Bundle 'altercation/vim-colors-solarized'
        Bundle 'bling/vim-airline'

    " }}}
    " Syntax {{{

        Bundle 'tpope/vim-markdown'

    " }}}
    " Haskell {{{

        Bundle 'syntaxhaskell.vim'
        Bundle 'indenthaskell.vim'
        Bundle 'lukerandall/haskellmode-vim'

    " }}}
    " Programming {{{

        " A better paste.
        Bundle 'sickill/vim-pasta'
        Bundle 'mileszs/ack.vim'
        Bundle 'majutsushi/tagbar'
        Bundle 'scrooloose/syntastic'
        Bundle 'tomtom/tcomment_vim'
        Bundle 'Valloric/YouCompleteMe'
        " Javascript tab completion
        " Bundle 'marijnh/tern_for_vim'
        Bundle 'kien/rainbow_parentheses.vim'
        Bundle 'tpope/vim-surround'
        Bundle 'Raimondi/delimitMate'
        " Automatically closes functions, blocks, etc.
        Bundle 'tpope/vim-endwise'
        Bundle 'kana/vim-textobj-user'
        Bundle 'michaeljsmith/vim-indent-object'
        " Configures % to match more than just single characters.
        Bundle 'vim-scripts/matchit.zip'
        Bundle 'SirVer/ultisnips'
        Bundle 'honza/vim-snippets'
        Bundle 'digitaltoad/vim-jade'

    " }}}
    " PHP {{{
        Bundle 'spf13/PIV'
    " }}}
    " Git {{{

        Bundle 'tpope/vim-git'
        Bundle 'tpope/vim-fugitive'
        Bundle 'int3/vim-extradite'
        Bundle 'mattn/gist-vim'
        Bundle 'airblade/vim-gitgutter'

    " }}}
    " Web Development {{{

        Bundle 'rstacruz/sparkup'
        Bundle 'othree/html5.vim'
        Bundle 'amirh/HTML-AutoCloseTag'
        Bundle 'https://github.com/ChrisYip/Better-CSS-Syntax-for-Vim.git'
        Bundle 'groenewege/vim-less'
        Bundle 'pangloss/vim-javascript'

    " }}}

" }}}
" General Settings -------------------------------------------------------- {{{

    set modelines=0
    set undolevels=1000
    set history=50
    set encoding=utf8
    " Share the clipboard.
    " set clipboard+=unnamed
    set colorcolumn=80
    set backspace=indent,eol,start
    set backup
    set backupdir^=$HOME/.vim/backup//
    set dir^=$HOME/.vim/swap//
    set undodir^=$HOME/.vim/undo//
    set viewdir^=$HOME/.vim/view//
    " Save undo tree.
    set undofile
    " Allow undoing a reload from disk.
    set undoreload=1000
    " Auto read externally modified files.
    set autoread
    " Auto write before certain commands.
    set autowrite
    set spelllang=en_ca
    " set shell=bash
    let mapleader = ','
    let maplocalleader = ';'
    set winminheight=1
    set shortmess=aIoOtT
    set showmode
    set showcmd
    " Scroll n lines before vertical edge.
    set scrolloff=3
    " Scroll n lines before horizontal edge.
    set sidescroll=3
    set visualbell
    " Don't keep windows at equal size.
    set noequalalways
    set splitbelow
    set splitright
    " Line break at the characters in breakat.
    set linebreak
    " Show ↪ at the beginning of wrapped lines.
    let &showbreak=nr2char(8618).' '
    " Fast scrolling when on a decent connection.
    set ttyfast
    " Insert mode completion.
    set completeopt=menuone,preview

    " Do not draw while executing macros.
    " Note: Enabling this will lag startup display if Powerline is enabled.
    " set lazyredraw

    " Set keys move cursor to next/previous line.
    set ww+=<,>,[,]
    set ruler
    set relativenumber
    set hidden
    set showmatch
    " Match for 3 tenths of a second.
    set matchtime=3
    " Pairs to match.
    set matchpairs+=<:>
    set printoptions+=syntax:y
    set printoptions+=number:y
    " Enable error jumping.
    set cf
    " Set diff fill char.
    set fillchars+=diff:⣿
    if has('mouse')
        set mouse=a
    endif
    syntax on
    filetype plugin indent on
    set background=dark
    colorscheme solarized

" }}}
" Window Title ------------------------------------------------------------ {{{

    if has('title') && (has('gui_running') || &title)
        " Set the title.
        set titlestring=
        " File name.
        set titlestring+=%f\ "
        " Flags.
        set titlestring+=%h%m%r%w
        " Program name.
        set titlestring+=\ -\ %{v:progname}
        " Working directory.
        set titlestring+=\ -\ %{substitute(getcwd(),\ $HOME,\ '~',\ '')}
    endif

" }}}
" Terminal Interface ------------------------------------------------------ {{{

    if &term =~ 'xterm'
        if &termencoding == ''
            set termencoding=utf-8
        endif

        if has('title')
            " Restore the title of the shell upon exit.
            let &titleold = 'Terminal'

            " Set the title.
            set title
        endif
        if has('mouse')
            " Terminal type for mouse recognition.
            set ttymouse=xterm2
        endif
        " Restore screen on exit.
        set restorescreen
        " Terminal in 'termcap' mode.
        set t_ti=7[r[?47h
        " Terminal out of 'termcap mode.
        set t_te=[?47l8

        " Tmux will only forward escape sequences to the terminal if surrounded by
        " a DCS sequence (http://bit.ly/zImrzb).
        if exists('$TMUX')
            let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
            let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
        else
            let &t_SI = "\<Esc>]50;CursorShape=1\x7"
            let &t_EI = "\<Esc>]50;CursorShape=0\x7"
        endif
    endif

" }}}
" Status Line ------------------------------------------------------------- {{{

    " Always show status.
    set laststatus=2
    " Disable status line fill chars.
    set fillchars+=stl:\ ,stlnc:\ " Space.

" }}}
" Search and Replace ------------------------------------------------------ {{{

    " Show partial matches as search is entered.
    set incsearch
    set hlsearch
    set ignorecase
    " Disable case insensitivity if mixed case.
    set smartcase
    " Wrap to top of buffer when searching.
    set wrapscan
    " Make search and replace global by default.
    set gdefault

    " Use sane regexes.
    nnoremap / /\v
    vnoremap / /\v
    nnoremap ? ?\v
    vnoremap ? ?\v

    " Keep search matches in the middle of the window.
    nnoremap * *zzzv
    nnoremap # #zzzv
    nnoremap n nzzzv
    nnoremap N Nzzzv

    " Keep jumps in the middle of the window.
    nnoremap g, g,zz
    nnoremap g; g;zz

    " Open a QuickFix window for the last search.
    nnoremap <silent> <Leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

    " Ack last search.
    nnoremap <silent> <Leader>? :execute "Ack! '" .
        \ substitute(
            \ substitute(
                \ substitute(
                    \ @/, "\\\\<", "\\\\b", ""),
                \ "\\\\>", "\\\\b", ""),
            \ "\\\\v", "", "") .
        \ "'"<CR>

" }}}
" Whitespace -------------------------------------------------------------- {{{

    " Do not select the end of line.
    " XXX: Using 'old' breaks UltiSnips by selecting one character past the
    " placeholder.
    "
    " set selection=old
    set noexpandtab
    set tabstop=8
    " Set soft tabs equal to 4 spaces.
    " set softtabstop=4
    " Set auto indent spacing.
    set shiftwidth=8
    " Shift to the next round tab stop.
    set shiftround
    " Insert spaces in front of lines.
    " set smarttab
    " Copy indent from the current line.
    set autoindent
    set wrap
    set textwidth=80
    set formatoptions=cqrn1
    set virtualedit+=block
    " Do not show invisible characters.
    set nolist
    " List of characters to show instead of whitespace.
    set listchars=tab:▸\ ,eol:¬,trail:⌴,extends:❯,precedes:❮
    " Highlight VCS conflict markers.
    match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" }}}
" Folding ----------------------------------------------------------------- {{{

    set foldenable
    set foldmethod=syntax
    set foldnestmax=2

    " Make zO recursively open the top level fold regardless of cursor placement.
    nnoremap zO zCzO

    highlight Folded term=bold cterm=bold ctermfg=10 ctermbg=8

    " "Focus" the current line.  Basically:
    "
    " 1. Close all folds.
    " 2. Open just the folds containing the current line.
    " 3. Move the line to a little bit (15 lines) above the center of the screen.
    "
    " This mapping wipes out the z mark, which I never use.
    nnoremap <Leader>z mzzMzvzz15<c-e>`z

    function! MyFoldText() " {{{
        let line = getline(v:foldstart)

        let nucolwidth = &fdc + (&number || &relativenumber) * &numberwidth
        let windowwidth = winwidth(0) - nucolwidth - 3
        let foldedlinecount = v:foldend - v:foldstart

        " expand tabs into spaces
        let onetab = strpart('          ', 0, &tabstop)
        let line = substitute(line, '\t', onetab, 'g')

        let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
        let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
        return line . '…' . repeat(" ",fillcharcount) . foldedlinecount . '…' . ' '
    endfunction " }}}
    set foldtext=MyFoldText()

" }}}
" File Name Auto Completion ----------------------------------------------- {{{

    " Show a list entries.
    set wildmenu

    " Wildcard expansion completion.
    set wildmode=list:longest,full

    set wildignore+=.hg,.git,.svn
    set wildignore+=node_modules
    set wildignore+=*.aux,*.out,*.toc
    set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
    set wildignore+=*.luac
    set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest
    set wildignore+=*.pyc
    set wildignore+=*.spl
    set wildignore+=*~,#*#,*.sw?,%*,*=
    set wildignore+=*.DS_Store

" }}}
" Auto Commands ----------------------------------------------------------- {{{

    " Auto save on lost focus. DISABLED: Closes help file when focus is lost.
    " au FocusLost * silent bufdo if !empty(bufname('%')) && !&ro | update | endif
    " au FocusLost * :wa

    " Remember folds."
    set viewoptions=cursor,folds
    au BufWinLeave * silent! mkview
    au BufWinEnter * silent! loadview

    " Switch the CWD to the current buffer.
    " au BufEnter * lcd %:p:h

    " Resize splits when the window is resized.
    au VimResized * exe "normal! \<c-w>="

    " Strip trailing whitespace.
    au BufWritePre,FileWritePre,FileAppendPre,FilterWritePre *
        \ call StripTrailingWhitespace()

    " Cursorline {{{

        " Highlight the current line in the current window.
        aug cursorline
            au!
            au BufEnter * set cursorline
            au BufLeave * set nocursorline
            au InsertEnter * set nocursorline
            au InsertLeave * set cursorline
        aug end

    " }}}
    " Trailing Whitespace {{{

        aug trailing
            au!
            au InsertEnter * :set listchars-=trail:⌴
            au InsertLeave * :set listchars+=trail:⌴
        aug end

    " }}}

" }}}
" File Settings ----------------------------------------------------------- {{{

    " BASH {{{

        aug ft_bash
            au!
            au BufNewFile,BufRead bash-fc-* setlocal filetype=sh
            " setlocal tabstop=2 softtabstop=2 shiftwidth=2
        aug end

    " }}}
    " CSS {{{

        aug ft_css
            au!
            au BufNewFile,BufRead *.less setlocal filetype=less

            " Use cc to change lines without borking the indentation.
            au BufNewFile,BufRead *.css,*.less nnoremap <buffer> cc ddko

            " Use <Leader>S to sort properties.
            au BufNewFile,BufRead *.css,*.less
                \ nnoremap <buffer> <LocalLeader>S ?{<CR>jV/\v^\s*\}?$<CR>k:sort<CR>:noh<CR>

            " Make {<CR> insert a pair of brackets in such a way that the cursor is
            " correctly positioned inside of them and the following code doesn't get
            " unfolded.
            au BufNewFile,BufRead *.css,*.less
                \ inoremap <buffer> {<CR> {}<left><CR>.<CR><esc>kA<bs><tab>
        aug end

    " }}}
    " Git {{{

        aug ft_git
            au!
            au FileType git* setlocal noexpandtab tabstop=4 shiftwidth=4 nofoldenable textwidth=72

        " Fugitive {{{

            " Jump to the last known position when reopening a file.
            au BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \   exe "normal! g'\"" |
            \ endif

            " Map '..' to go up a directory in fugitive tree/blob buffers.
            au User fugitive
            \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
            \   nnoremap <buffer> .. :edit %:h<CR> |
            \ endif

            " Auto-clean fugitive buffers.
            au BufReadPost fugitive://* set bufhidden=delete

        " }}}

        aug end

    " }}}
    " Handlebars {{{

        aug ft_handlebars
            au!
            au FileType handlebars compiler html
            au BufNewFile,BufRead *.handlebars setlocal filetype=handlebars syntax=html
        aug end

    " }}}
    " HTML {{{

        aug ft_html
            au!
            au FileType html compiler html
            au FileType html ru ftplugin/html_autoclosetag.vim
        aug end

    " }}}
    " Haskell {{{

        aug ft_haskell
            au!
            au FileType haskell compiler ghc
            au FileType haskell
                \ setlocal sw=4 et sts=4 ts=8 tw=79 cc=80
                "          |    |  |     \_ Respect files with actual tabs in them,
                "          |    |  \_______ but our 'tabs' are 4 positions...
                "          |    \__________ and are not really tabs, but spaces.
                "          |
                "          \_______________ Indent in steps of 4 spaces.
        aug end

    " }}}
    " Mail {{{

        aug ft_mail
            au!
            au Filetype mail setlocal spell
        aug end

    " }}}
    " Markdown {{{

        aug ft_markdown
            au!
            au BufNewFile,BufRead *.m*down setlocal filetype=markdown
        aug end

    " }}}
    " Mercurial {{{

        aug ft_mercurial
            au!
            au BufNewFile,BufRead .hgrc,hgrc,Mercurial.ini setlocal filetype=dosini
        aug end

    " }}}
    " Python {{{

        aug ft_python
            au!
            au FileType python
                \ noremap <buffer> <LocalLeader>rr :RopeRename<CR>
                \ vnoremap <buffer> <LocalLeader>rm :RopeExtractMethod<CR>
                \ noremap <buffer> <LocalLeader>ri :RopeOrganizeImports<CR>
                \ setlocal
                    \ omnifunc=pythoncomplete#Complete
                    \ tabstop=4
                    \ softtabstop=4
                    \ shiftwidth=4
                    \ textwidth=79
                    \ colorcolumn=80

                " Pydoc
                au FileType python noremap <buffer> <
                au FileType python noremap <buffer> <LocalLeader>ds :call ShowPyDoc('<C-R><C-W>', 1)<CR>
        aug end

    " }}}
    " Python (Django) {{{

        aug ft_django
            au!
            au BufNewFile,BufRead dashboard.py normal! zR
            au BufNewFile,BufRead settings.py setlocal foldmethod=marker
            au BufNewFile,BufRead urls.py
                \ setlocal nowrap
                \ normal! zR
            au BufNewFile,BufRead admin.py,urls.py,models.py,views.py,settings.py,forms.py
                \ setlocal filetype=python.django
            au BufNewFile,BufRead common_settings.py
                \ setlocal filetype=python.django foldmethod=marker
        aug end

    " }}}
    " QuickFix {{{

        aug ft_quickfix
            au!
            au Filetype qf setlocal colorcolumn=0 nolist nocursorline nowrap
        aug end

    " }}}
    " Text {{{

        aug ft_text
            au!
            " Enable soft-wrapping for text files
            au FileType text,markdown,handlebars,html,xhtml,eruby setlocal wrap linebreak nolist
        aug end

    " }}}
    " Vagrant {{{

        aug ft_vagrant
            au!
            au BufRead,BufNewFile Vagrantfile set filetype=ruby
        aug end

    " }}}
    " Vim {{{

        aug ft_vim
            au!
            au FileType vim,help setlocal textwidth=78
            au FileType vim setlocal foldmethod=marker colorcolumn=79
        aug end

    " }}}
    " Zsh {{{

        aug ft_zsh
            au!
            au BufNewFile,BufRead zshecl*,prompt_*_setup setlocal filetype=zsh
            " setlocal tabstop=2 softtabstop=2 shiftwidth=2
        aug end

    " }}}

" }}}
" Plugin Settings --------------------------------------------------------- {{{

    let g:delimitMate_expand_cr = 1
    let g:delimitMate_expand_space = 1
    let g:delimitMate_jump_expansion = 1

    " Ack {{{

        map <Leader>a :Ack!

    " }}}
    " Airline {{{
        let g:airline#extensions#tabline#enabled = 1
    " }}}
    " Auto Complete Pop {{{

        " Set length of characters before keyword completion.
        let g:AutoComplPop_BehaviorKeywordLength = 4

    " }}}
    " Extradite {{{

        " Show the commit hash.
        let g:extradite_showhash = 1

    " }}}
    " CtrlP {{{

        " Go up the file system until '.git', or similar, is found.
        let g:ctrlp_working_path_mode = 'raw'

        " Set the maximum height of the match window.
        let g:ctrlp_max_height = 10

        " Do not remember the last input.
        let g:ctrlp_persistent_input = 0

        " Do not override Ctrl + P.
        let g:ctrlp_map = '<Leader>t'

        " Update results after typing has stopped.
        let g:ctrlp_lazy_update = 1

        " Enable help tag, exuberant ctags, quickfix, and directory search.
        let g:ctrlp_extensions = ['tag', 'buffertag', 'quickfix', 'dir']

        " Use more restrictive/faster VCS file listing
        let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files']

        " Map buffer search.
        nnoremap <Leader>b :CtrlPBuffer<CR>

        " Map most recently used file search.
        nnoremap <Leader>m :CtrlPMRU<CR>

        " Map clear cache.
        nnoremap <Leader>c :CtrlPClearCache<CR>

    " }}}
    " Gist {{{

        let g:gist_clip_command = 'pbcopy'
        let g:gist_detect_filetype = 1
        let g:gist_open_browser_after_post = 1

    " }}}
    " Git Gutter {{{

        hi SignColumn ctermbg=8

    " }}}
    " Golden Ratio {{{

        nnoremap <Leader>G <Plug>(golden_ratio_resize)
        let g:golden_ratio_autocommand = 0

    " }}}
    " Gundo {{{

        nnoremap <Leader>U :GundoToggle<CR>
        let g:gundo_preview_bottom = 1

    " }}}
    " Haskellmode {{{

        let g:haddock_browser = "firefox-bin"
        let g:ghc = "ghc"

    " }}}
    " HTML5 {{{

        let g:event_handler_attributes_complete = 0
        let g:rdfa_attributes_complete = 0
        let g:microdata_attributes_complete = 0
        let g:atia_attributes_complete = 0

    " }}}
    " Indent Guides {{{

        " Auto calculate guide colors.
        let g:indent_guides_auto_colors = 1

        " Use skinny guides.
        let g:indent_guides_guide_size = 1

        " Indent from level 2.
        let g:indent_guides_start_level = 1

    " }}}
    " Org-Mode {{{

        let g:org_todo_keywords = ['TODO', '|', 'DONE']
        let g:org_plugins = [
            \ 'ShowHide', '|', 'Navigator',
            \ 'EditStructure', '|', 'Todo', 'Date', 'Misc'
        \ ]

    " }}}
    " Preview {{{

        let g:PreviewBrowsers='open'

    " }}}
    " Python by Dmitry Vasiliev {{{

        let python_highlight_all = 1
        let python_print_as_function = 1
        let python_slow_sync = 1

    " }}}
    " Rainbow Parenthesis {{{

        nmap <Leader>rp :RainbowParenthesesToggle<CR>

    " }}}
    " Ropevim {{{

        let ropevim_enable_shortcuts = 0
        let ropevim_guess_project = 1
        let ropevim_global_prefix = '<C-c>p'

    " }}}
    " Showmarks {{{

        " Do not enable showmarks at startup.
        let g:showmarks_enable = 0

        " Do not include the various brace marks (), {}, etc.
        let g:showmarks_include = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXY"

    " }}}
    " Solarized {{{

        if exists('g:colors_name') && g:colors_name == 'solarized'
            " Text is unreadable with background transparency.
            if has('gui_macvim')
                set transparency=0
            endif

            " Highlighted text is unreadable in Terminal.app because it
            " does not support setting of the cursor foreground color.
            if !has('gui_running') && $TERM_PROGRAM == 'Apple_Terminal'
                if &background == 'dark'
                    hi Visual term=reverse cterm=reverse ctermfg=10 ctermbg=7
                endif
            endif
        endif

    " }}}
    " Surround {{{

        let g:surround_40 = "(\r)"
        let g:surround_91 = "[\r]"
        let g:surround_60 = "<\r>"

    " }}}
    " Syntastic {{{

        " Mark syntax errors with :signs.
        let g:syntastic_enable_signs = 1

        " Do not auto jump to the error when saving a file.
        let g:syntastic_auto_jump = 0

        " Do not auto show the error list.
        let g:syntastic_auto_loc_list = 0

        " Show warnings.
        let g:syntastic_quiet_warnings = 0

        " Do not validate the following file types.
        let g:syntastic_disabled_filetypes = ['html', 'python']

        " Set the display format.
        let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'

    " }}}
    " Tagbar {{{

        let g:tagbar_sort = 1            " Sort by name.
        let g:tagbar_compact = 0         " Use compact layout.
        let g:tagbar_expand = 1          " Expand window in GUI mode.

        map <Leader>i <Plug>TagbarToggle

        " Define custom Zsh support (requires definition in ~/.ctags).
        let g:tagbar_type_zsh = {
            \ 'ctagstype': 'zsh',
            \ 'kinds': [
                \ 'f:functions:1'
            \ ],
            \ 'fold': 0
        \ }

    " }}}
    " TaskList {{{

        map <Leader>T <Plug>TaskList

    " }}}
    " Tcomment {{{

        " Map 'gcp' comment the current paragraph (block).
        if exists('loaded_tcomment')
            nmap <silent> gcp <c-_>p
        endif

    " }}}
    " UltiSnips {{{

        " Only use honza/vim-snippets
        let g:UltiSnipsSnippetDirectories = ["../vim-snippets/UltiSnips"]

        let g:UltiSnipsSnippetsDir = "~/.vim/UltiSnips"

        let g:UltiSnipsExpandTrigger = "<Nul>"
        let g:UltiSnipsJumpForwardTrigger = "<Nul>"

        let g:ulti_expand_res = 0
        function! UltiSnips_MaybeExpandSnippet()
            call UltiSnips_ExpandSnippet()
            return g:ulti_expand_res
        endfunction

        let g:ulti_jump_forwards_res = 0
        function! UltiSnips_MaybeJumpForwards()
            call UltiSnips_JumpForwards()
            return g:ulti_jump_forwards_res
        endfunction

        let g:ulti_jump_backwards_res = 0
        function! UltiSnips_MaybeJumpBackwards()
            call UltiSnips_JumpBackwards()
            return g:ulti_jump_backwards_res
        endfunction

        function! g:Wolfjourn_HandleCR()
            if !pumvisible()
                return "\<Esc>"
            elseif UltiSnips_MaybeExpandSnippet()
                return ""
            else
                " Close completion menu
                " Use feedkeys since we need mappings
                call feedkeys("\<Plug>ToggleYCM\<C-y>\<Plug>ToggleYCM")
                return ""
            endif
        endfunction

        function! g:Wolfjourn_HandleTab()
            if pumvisible()
                return "\<C-n>"
            elseif UltiSnips_MaybeJumpForwards()
                return ""
            else
                return "\<Tab>"
            endif
        endfunction

        function! g:Wolfjourn_HandleShiftTab()
            if pumvisible()
                return "\<C-p>"
            elseif UltiSnips_MaybeJumpBackwards()
                return ""
            else
                return "\<S-Tab>"
            endif
        endfunction

        inoremap <CR> <C-R>=g:Wolfjourn_HandleCR()<CR>
        inoremap <Tab> <C-R>=g:Wolfjourn_HandleTab()<CR>
        inoremap <S-Tab> <C-R>=g:Wolfjourn_HandleShiftTab()<CR>

        snoremap <Tab> <Esc>:call UltiSnips_JumpForwards()<CR>
        snoremap <S-Tab> <Esc>:call UltiSnips_JumpBackwards()<CR>

    " }}}
    " Yankring {{{

        " Hide the history file.
        let g:yankring_history_file = '.yankring-history'

    " }}}
    " YouCompleteMe {{{

        let g:ycm_min_num_of_chars_for_completion = 1

        " YCM will hose <Tab> bindings after .vimrc unless we redefine this list
        let g:ycm_key_list_select_completion = ['<Down>']
        " Same for <S-Tab>
        let g:ycm_key_list_previous_completion = ['<Up>']

        function! g:Wolfjourn_ToggleYCM()
            if &completefunc != ''
                let g:tmp_completefunc = &completefunc
                let &completefunc = ''
            else
                let &completefunc = g:tmp_completefunc
            endif

            return ""
        endfunction

        inoremap <expr> <Plug>ToggleYCM g:Wolfjourn_ToggleYCM()

        function! g:FixCtrlY()
            if pumvisible()
                call feedkeys("\<Plug>ToggleYCM")
                call feedkeys("\<C-y>", 'n')
                call feedkeys("\<Plug>ToggleYCM")
            else
                call feedkeys("\<C-y>", 'n')
            endif

            return ''
        endfunction

        inoremap <expr> <C-y> g:FixCtrlY()

    " }}}
    " NERDTree {{{

        let g:NERDShutUp = 1
        map <C-e> :NERDTreeToggle<CR>:NERDTreeMirror<CR>
        map <leader>e :NERDTreeFind<CR>
        nmap <leader>nt :NERDTreeFind<CR>

        let NERDTreeShowBookmarks=1
        let NERDTreeIgnore=['\.pyc', '\~$', '\.swo$', '\.swp$', '\.git', '\.hg', '\.svn', '\.bzr']
        let NERDTreeChDirMode=0
        let NERDTreeQuitOnOpen=1
        let NERDTreeMouseMode=2
        let NERDTreeShowHidden=1
        let NERDTreeKeepTreeInNewTab=1
        let g:nerdtree_tabs_open_on_gui_startup=0

    " }}}

" }}}
" Key Remapping ----------------------------------------------------------- {{{

    " Easily exit insert mode (DISABLED: This is handled in
    " g:Wolfjourn_HandleCR)
    "
    " inoremap <CR> <Esc>

    " <S-CR> is macroed to <C-b> on my Kinesis Advantage, which enables us to
    " get around the problem of terminals sending <CR> for <C-CR>/<S-CR>. The
    " net effect being that <CR> exits insert mode, while <S-CR> inserts
    " a newline.
    inoremap <C-b> <CR>

    nnoremap <Leader>s :w<CR>
    nnoremap <Leader>q :wq<CR>

    " Tab Navigation {{{

        " Easily create a new tab.
        map <Leader>tt :tabnew<CR>

        " Easily close a tab.
        map <Leader>tc :tabclose<CR>

        " Easily move a tab.
        noremap <Leader>tm :tabmove<CR>

        " Easily go to next tab.
        noremap <Leader>tn :tabnext<CR>

        " Easily go to previous tab.
        noremap <Leader>tp :tabprevious<CR>

    " }}}
    " Window Navigation {{{

        " Navigate to left window.
        nnoremap <C-h> <C-w>h

        " Navigate to down window.
        nnoremap <C-j> <C-w>j

        " Navigate to top window.
        nnoremap <C-k> <C-w>k

        " Navigate to right window.
        nnoremap <C-l> <C-w>l

        " Horizontal split then move to bottom window.
        nnoremap <Leader>- <C-w>s

        " Vertical split then move to right window.
        nnoremap <Leader>\| <C-w>v<C-w>l

    " }}}
    " Text Alignment {{{

        nnoremap <Leader>Al :left<CR>
        nnoremap <Leader>Ac :center<CR>
        nnoremap <Leader>Ar :right<CR>
        vnoremap <Leader>Al :left<CR>
        vnoremap <Leader>Ac :center<CR>
        vnoremap <Leader>Ar :right<CR>

    " }}}
    " Text Movement {{{

        noremap <Leader>j :m+<CR>
        noremap <Leader>k :m-2<CR>
        vnoremap <Leader>j :m'>+<CR>gv
        vnoremap <Leader>k :m-2<CR>gv

    " }}}
    " Miscellaneous Mappings {{{

        " Disable search match highlight.
        nnoremap <Leader><space> :noh<CR>

        " Duplicate a selection.
        vnoremap D y'>p

        " Tab to indent in visual mode.
        " vnoremap <Tab> >gv

        " Shift+Tab to unindent in visual mode.
        " vnoremap <S-Tab> <gv

        " Re hard wrap paragraph.
        " nnoremap <Leader>q gqip

        " Reselect pasted text.
        nnoremap <Leader>v V`]

        " Reselect text ater indent/unindent.
        vnoremap < <gv
        vnoremap > >gv

        " Display-wise up/down movement instead of linewise.
        noremap j gj
        noremap k gk

        " Faster ESC.
        inoremap jk <ESC>
        inoremap kj <ESC>
        inoremap jj <ESC>
        inoremap kk <ESC>

        " Make Ctrl-C trigger InsertLeave autocmds
        "
        " This is very heavy-handed, but Ctrl-C is what I'm accustomed to using to
        " leave insert mode.
        inoremap <C-C> <ESC>

        " Format Paragraph.
        " nnoremap <Leader>q gwap

        " Formatting, TextMate-style.
        nnoremap Q gqip

        " Change Case.
        nnoremap <C-u> gUiw
        inoremap <C-u> <ESC>gUiwea

        " Write with sudo.
        cnoremap w!! w !sudo tee % >/dev/null

        " Toggle spell checking.
        " nnoremap <silent><Leader>s :set spell!<CR>

        " Shift+P replace selection without overwriting default register in vmode.
        vnoremap P p :call setreg('"', getreg('0'))<CR>

        " Strip trailing whitespace.
        nnoremap <Leader>W call StripTrailingWhiteSpace()

        " Quick return.
        inoremap <C-CR> <ESC>A<CR>
        inoremap <S-C-CR> <ESC>A:<CR>

        " Fix linewise visual selection of various text objects.
        nnoremap VV V
        nnoremap Vit vitVkoj
        nnoremap Vat vatV
        nnoremap Vab vabV
        nnoremap VaB vaBV

        " Faster substitute.
        nnoremap <Leader>S :%s//<left>

        " Easier linewise reselection.
        nnoremap <Leader>v V`]

        " Toggle paste.
        set pastetoggle=<F12>

        " Replaste.
        nnoremap <D-p> "_ddPV`]

        " Diff.
        nnoremap <silent><Leader>do :diffoff!<CR>
        nnoremap <silent><Leader>dg :diffget<CR>:diffupdate<CR>
        nnoremap <silent><Leader>dp :diffput<CR>:diffupdate<CR>
        nnoremap <silent><Leader>du :diffupdate<CR>

        " Better completion.
        " inoremap <expr> <CR>  pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
        " inoremap <expr> <C-p> pumvisible() ?
        "     \ '<C-n>'  : '<C-n><C-r>=pumvisible() ?
        "         \ "\<lt>up>" : ""<CR>'
        " inoremap <expr> <C-n> pumvisible() ?
        "     \ '<C-n>'  : '<C-n><C-r>=pumvisible() ?
        "     \ "\<lt>Down>" : ""<CR>'

    " }}}

" }}}
" Text Objects (Credit: Steve Losh) --------------------------------------- {{{

    " Shortcut for [] {{{

        onoremap id i[
        onoremap ad a[
        vnoremap id i[
        vnoremap ad a[

    " }}}
    " Next/Last () {{{

        vnoremap <silent> inb :<C-U>normal! f(vib<CR>
        onoremap <silent> inb :<C-U>normal! f(vib<CR>
        vnoremap <silent> anb :<C-U>normal! f(vab<CR>
        onoremap <silent> anb :<C-U>normal! f(vab<CR>
        vnoremap <silent> in( :<C-U>normal! f(vi(<CR>
        onoremap <silent> in( :<C-U>normal! f(vi(<CR>
        vnoremap <silent> an( :<C-U>normal! f(va(<CR>
        onoremap <silent> an( :<C-U>normal! f(va(<CR>

        vnoremap <silent> ilb :<C-U>normal! F)vib<CR>
        onoremap <silent> ilb :<C-U>normal! F)vib<CR>
        vnoremap <silent> alb :<C-U>normal! F)vab<CR>
        onoremap <silent> alb :<C-U>normal! F)vab<CR>
        vnoremap <silent> il( :<C-U>normal! F)vi(<CR>
        onoremap <silent> il( :<C-U>normal! F)vi(<CR>
        vnoremap <silent> al( :<C-U>normal! F)va(<CR>
        onoremap <silent> al( :<C-U>normal! F)va(<CR>

    " }}}
    " Next/Last {} {{{

        vnoremap <silent> inB :<C-U>normal! f{viB<CR>
        onoremap <silent> inB :<C-U>normal! f{viB<CR>
        vnoremap <silent> anB :<C-U>normal! f{vaB<CR>
        onoremap <silent> anB :<C-U>normal! f{vaB<CR>
        vnoremap <silent> in{ :<C-U>normal! f{vi{<CR>
        onoremap <silent> in{ :<C-U>normal! f{vi{<CR>
        vnoremap <silent> an{ :<C-U>normal! f{va{<CR>
        onoremap <silent> an{ :<C-U>normal! f{va{<CR>

        vnoremap <silent> ilB :<C-U>normal! F}viB<CR>
        onoremap <silent> ilB :<C-U>normal! F}viB<CR>
        vnoremap <silent> alB :<C-U>normal! F}vaB<CR>
        onoremap <silent> alB :<C-U>normal! F}vaB<CR>
        vnoremap <silent> il{ :<C-U>normal! F}vi{<CR>
        onoremap <silent> il{ :<C-U>normal! F}vi{<CR>
        vnoremap <silent> al{ :<C-U>normal! F}va{<CR>
        onoremap <silent> al{ :<C-U>normal! F}va{<CR>

    " }}}
    " Next/Last [] {{{

        vnoremap <silent> ind :<C-U>normal! f[vi[<CR>
        onoremap <silent> ind :<C-U>normal! f[vi[<CR>
        vnoremap <silent> and :<C-U>normal! f[va[<CR>
        onoremap <silent> and :<C-U>normal! f[va[<CR>
        vnoremap <silent> in[ :<C-U>normal! f[vi[<CR>
        onoremap <silent> in[ :<C-U>normal! f[vi[<CR>
        vnoremap <silent> an[ :<C-U>normal! f[va[<CR>
        onoremap <silent> an[ :<C-U>normal! f[va[<CR>

        vnoremap <silent> ild :<C-U>normal! F]vi[<CR>
        onoremap <silent> ild :<C-U>normal! F]vi[<CR>
        vnoremap <silent> ald :<C-U>normal! F]va[<CR>
        onoremap <silent> ald :<C-U>normal! F]va[<CR>
        vnoremap <silent> il[ :<C-U>normal! F]vi[<CR>
        onoremap <silent> il[ :<C-U>normal! F]vi[<CR>
        vnoremap <silent> al[ :<C-U>normal! F]va[<CR>
        onoremap <silent> al[ :<C-U>normal! F]va[<CR>

    " }}}
    " Next/Last <> {{{

        vnoremap <silent> in< :<C-U>normal! f<vi<<CR>
        onoremap <silent> in< :<C-U>normal! f<vi<<CR>
        vnoremap <silent> an< :<C-U>normal! f<va<<CR>
        onoremap <silent> an< :<C-U>normal! f<va<<CR>

        vnoremap <silent> il< :<C-U>normal! f>vi<<CR>
        onoremap <silent> il< :<C-U>normal! f>vi<<CR>
        vnoremap <silent> al< :<C-U>normal! f>va<<CR>
        onoremap <silent> al< :<C-U>normal! f>va<<CR>

    " }}}
    " Next '' {{{

        vnoremap <silent> in' :<C-U>normal! f'vi'<CR>
        onoremap <silent> in' :<C-U>normal! f'vi'<CR>
        vnoremap <silent> an' :<C-U>normal! f'va'<CR>
        onoremap <silent> an' :<C-U>normal! f'va'<CR>

        vnoremap <silent> il' :<C-U>normal! F'vi'<CR>
        onoremap <silent> il' :<C-U>normal! F'vi'<CR>
        vnoremap <silent> al' :<C-U>normal! F'va'<CR>
        onoremap <silent> al' :<C-U>normal! F'va'<CR>

    " }}}
    " Next "" {{{

        vnoremap <silent> in" :<C-U>normal! f"vi"<CR>
        onoremap <silent> in" :<C-U>normal! f"vi"<CR>
        vnoremap <silent> an" :<C-U>normal! f"va"<CR>
        onoremap <silent> an" :<C-U>normal! f"va"<CR>

        vnoremap <silent> il" :<C-U>normal! F"vi"<CR>
        onoremap <silent> il" :<C-U>normal! F"vi"<CR>
        vnoremap <silent> al" :<C-U>normal! F"va"<CR>
        onoremap <silent> al" :<C-U>normal! F"va"<CR>

    " }}}

" }}}
" Abbreviations ----------------------------------------------------------- {{{

    cabbr cdf cd %:p:h<CR>
    cabbr lcdf lcd %:p:h<CR>
    cabbr cwd lcd %:p:h

" }}}
" Functions --------------------------------------------------------------- {{{

    " Open URL {{{

        command! -bar -nargs=1 OpenURL :!open <args>
        function! OpenURLUnderCursor()
            let l:uri = matchstr(getline("."), '[a-z]*:\/\/[^ >,;:]*')
            if l:uri != ""
                exec '!open "' . l:uri . '"'
            else
                echo 'No URL found in line'
            endif
        endfunction
        nmap <silent> <Leader>w :call OpenURLUnderCursor()<CR>

    " }}}
    " Error Toggle {{{

        command! ErrorsToggle call ErrorsToggle()
        function! ErrorsToggle()
            if exists("w:is_error_window")
                unlet w:is_error_window
                exec "q"
            else
                exec "Errors"
                lopen
                let w:is_error_window = 1
            endif
        endfunction

        command! -bang -nargs=? QFixToggle call QFixToggle(<bang>0)
        function! QFixToggle(forced)
            if exists("g:qfix_win") && a:forced == 0
                cclose
                unlet g:qfix_win
            else
                copen 10
                let g:qfix_win = bufnr("$")
            endif
        endfunction

        nmap <silent> <F3> :ErrorsToggle<CR>
        nmap <silent> <F4> :QFixToggle<CR>

    " }}}
    " Strip Trailing Whitespace {{{

        function! StripTrailingWhitespace()
            if !&binary && &modifiable && &filetype != 'diff'
                let l:winview = winsaveview()
                %s/\s\+$//e
                let @/=''
                call winrestview(l:winview)
            endif
        endfunction

    " }}}
    " Toggle Background {{{

        function! BackgroundToggle()
            let &background = ( &background == "dark"? "light" : "dark" )
            if exists("g:colors_name")
                exe "colorscheme " . g:colors_name
            endif
        endfunction
        command! BackgroundToggle :call BackgroundToggle()
        silent! nnoremap <F2> :BackgroundToggle<CR>
        silent! inoremap <F2> :BackgroundToggle<CR>
        silent! vnoremap <F2> :BackgroundToggle<CR>

    " }}}

" }}}
