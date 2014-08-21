" References:
"
" http://statico.github.io/vim2.html
" http://dougblack.io/words/a-good-vimrc.html
" http://stackoverflow.com/questions/19030463/how-do-you-work-with-large-projects-in-vim
" https://github.com/spf13/spf13-vim
" https://github.com/tpope/tpope/blob/master/.vimrc

" http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html

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

        Bundle 'flazz/vim-colorschemes'
        " Bundle 'altercation/vim-colors-solarized'
        " Bundle 'w0ng/vim-hybrid'
        " Bundle 'vim-scripts/ScrollColors'

        " Identifies file indentation.
        Bundle 'godlygeek/tabular'
        Bundle 'sjl/gundo.vim'
        " Show marks in a gutter.
        Bundle 'kshenoy/vim-signature'
        Bundle 'Lokaltog/vim-easymotion'
        " s?? to jump to next occurrence of ?? in the line
        Bundle 'goldfeld/vim-seek'
        " History of yanks, changes, and deletes.
        " Bundle 'vim-scripts/YankRing.vim'
        Bundle 'kien/ctrlp.vim'
        Bundle 'coderifous/textobj-word-column.vim'
        Bundle 'bling/vim-airline'
        " Bundle 'benmills/vimux'

    " }}}

    " tpope {{{
        Bundle 'tpope/vim-vividchalk'
        Bundle 'tpope/vim-sleuth'
        Bundle 'tpope/vim-repeat'
        " Provides pairs of bracket mappings for buffer, file navigation and editing.
        Bundle 'tpope/vim-unimpaired'
        Bundle 'tpope/vim-scriptease'
        " :Unlink/:Remove/:Move/etc
        Bundle 'tpope/vim-eunuch'
        " netrw via -
        Bundle 'tpope/vim-vinegar'
        " Easily search for, substitute, and abbreviate multiple variants of a word.
        " Also, coerce text: crs(nake)/crm(ixed)/cru(pper)/crc(amel)
        Bundle 'tpope/vim-abolish'
        " Continuously updated session files
        Bundle 'tpope/vim-obsession'
        " Bundle 'tpope/vim-tbone'
        " Bundle 'tpope/vim-jdaddy'
        Bundle 'tpope/vim-markdown'
        Bundle 'tpope/vim-rails'
        " Yes. Setup tmux to make this awesome. TODO
        " Bundle tpope/vim-dispatch
        Bundle 'tpope/vim-surround'
        Bundle 'tpope/vim-endwise'
        Bundle 'tpope/vim-git'
        Bundle 'tpope/vim-fugitive'
    " }}}

    " Haskell {{{

        Bundle 'syntaxhaskell.vim'
        Bundle 'indenthaskell.vim'

    " }}}
    " Programming {{{

        " Context aware paste (indentation)
        Bundle 'sickill/vim-pasta'
        Bundle 'mileszs/ack.vim'
        Bundle 'majutsushi/tagbar'
        Bundle 'scrooloose/syntastic'
        Bundle 'tomtom/tcomment_vim'

        " This shit is so slow and annoying.
        " Bundle 'Valloric/YouCompleteMe'

        " Javascript tab completion
        " Bundle 'marijnh/tern_for_vim'
        " Bundle 'kien/rainbow_parentheses.vim'
        Bundle 'Raimondi/delimitMate'
        " Automatically closes functions, blocks, etc.
        Bundle 'kana/vim-textobj-user'
        Bundle 'michaeljsmith/vim-indent-object'
        " Configures % to match more than just single characters.
        " Bundle 'vim-scripts/matchit.zip'
        " Disabled until I get around to properly integrating/learning this.
        " Bundle 'SirVer/ultisnips'
        " Bundle 'honza/vim-snippets'

    " }}}
    " Git {{{

        Bundle 'int3/vim-extradite'
        Bundle 'airblade/vim-gitgutter'

    " }}}
    " Web Development {{{

        " What does any of this shit actually accomplish?
        " Bundle 'rstacruz/sparkup'
        " Bundle 'othree/html5.vim'
        " Bundle 'amirh/HTML-AutoCloseTag'
        Bundle 'pangloss/vim-javascript'
        Bundle 'mustache/vim-mustache-handlebars'
        " Syntax highlighting for reddit mako templates
        " Bundle 'sophacles/vim-bundle-mako'

    " }}}

" }}}
" General Settings -------------------------------------------------------- {{{

    set modelines=0
    set undolevels=1000
    set history=50
    set encoding=utf8
    " Share the clipboard.
    " set clipboard+=unnamed
    set colorcolumn=+1
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
    set listchars=tab:▸\ ,trail:·
    set list
    " Fast scrolling when on a decent connection.
    set ttyfast
    " Insert mode completion.
    set completeopt=menuone,preview

    " Do not draw while executing macros.
    " Horrible lag when scrolling through ruby files with this disabled.
    " See: http://stackoverflow.com/questions/16902317/vim-slow-with-ruby-syntax-highlighting
    " http://stackoverflow.com/a/20519492
    set lazyredraw

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
    " set background=dark

    " let g:hybrid_use_Xresources = 1
    " colorscheme hybrid
    " colorscheme badwolf
    colorscheme vividchalk

    " hi LineNr ctermbg=0 ctermfg=8 cterm=none
    " hi CursorLine cterm=none

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

    set tabstop=4
    set softtabstop=4
    set expandtab
    " Do not select the end of line.
    " XXX: Using 'old' breaks UltiSnips by selecting one character past the
    " placeholder.
    "
    " set selection=old
    " Shift to the next round tab stop.
    set shiftround
    " Copy indent from the current line.
    set autoindent
    set wrap
    set textwidth=79
    set formatoptions=cqrn1
    set virtualedit+=block
    " Highlight VCS conflict markers.
    match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" }}}
" Folding ----------------------------------------------------------------- {{{

    set foldenable
    set foldmethod=syntax
    set foldnestmax=2
    set foldlevelstart=5

    " space open/closes fold
    nnoremap <space> za

    " Make zO recursively open the top level fold regardless of cursor placement.
    nnoremap zO zCzO

    highlight Folded cterm=NONE ctermfg=10 ctermbg=0

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

    " Switch the CWD to the current buffer.
    " au BufEnter * lcd %:p:h

    " Resize splits when the window is resized.
    au VimResized * exe "normal! \<c-w>="

    " Strip trailing whitespace.
    " au BufWritePre,FileWritePre,FileAppendPre,FilterWritePre *
    "     \ call StripTrailingWhitespace()

    " Cursorline {{{

        " Highlight the current line in the current window.
        aug cursorline
            au!
            au BufEnter * set cursorline
            au BufLeave * set nocursorline
            au InsertEnter * set nocursorline
            au InsertLeave * set cursorline
            hi CursorLine   ctermbg=235 cterm=NONE
            hi CursorLineNr ctermbg=235 ctermfg=10 cterm=italic
        aug end

    " }}}

" }}}
" File Settings ----------------------------------------------------------- {{{

    aug file_settings
        au!
        au BufNewFile,BufRead bash-fc-* setlocal filetype=sh

        " au BufNewFile,BufRead *.less setlocal filetype=less
        au BufNewFile,BufRead *.less setlocal filetype=css

        " Use <Leader>S to sort properties.
        au BufNewFile,BufRead *.css,*.less
            \ nnoremap <buffer> <LocalLeader>S ?{<CR>jV/\v^\s*\}?$<CR>k:sort<CR>:noh<CR>
        au FileType git* setlocal noexpandtab tabstop=4 shiftwidth=4 nofoldenable textwidth=72

        " Jump to the last known position when reopening a file.
        au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal! g'\"" |
        \ endif

        " au FileType handlebars compiler html
        au BufNewFile,BufRead *.handlebars setlocal filetype=handlebars syntax=html
        au BufNewFile,BufRead *.js.es6 setlocal filetype=javascript

        " au FileType html compiler html
        au FileType html ru ftplugin/html_autoclosetag.vim
        au BufNewFile,BufRead *.m*down setlocal filetype=markdown
        au Filetype qf setlocal colorcolumn=0 nolist nocursorline nowrap

        " Enable soft-wrapping for text files
        au FileType text,markdown,handlebars,html,xhtml,eruby setlocal wrap linebreak nolist
        au BufRead,BufNewFile Vagrantfile set filetype=ruby
        au FileType vim,help setlocal textwidth=78
        au FileType vim setlocal foldmethod=marker colorcolumn=79
        au BufNewFile,BufRead zshecl*,prompt_*_setup setlocal filetype=zsh
    aug end

" }}}
" Plugin Settings --------------------------------------------------------- {{{

    let g:delimitMate_expand_cr = 2
    let g:delimitMate_expand_space = 1

    " Don't jump over shit on other lines
    let g:delimitMate_jump_expansion = 0

    " Ack/The Silver Searcher {{{
        let g:ackprg = 'ag --nogroup --nocolor --column'
    " }}}
    " Airline {{{
        " let g:airline#extensions#tabline#enabled = 0

        " Showing git-gutter stats is mega slow
        let g:airline_section_b = ''
        let g:airline#extensions#hunks#enabled = 0
    " }}}
    " CtrlP {{{

        " Go up the file system until '.git', or similar, is found.
        let g:ctrlp_working_path_mode = 'raw'

        let g:ctrlp_max_height = 10
        let g:ctrlp_persistent_input = 0
        let g:ctrlp_map = '<Leader>t'
        let g:ctrlp_lazy_update = 1

        " Enable help tag, exuberant ctags, quickfix, and directory search.
        let g:ctrlp_extensions = ['tag', 'buffertag', 'quickfix', 'dir']

        let g:ctrlp_custom_ignore = {
            \ 'dir': '\.git$\|\.hg$\|\.svn$',
            \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$'
        \ }

        let g:ctrlp_user_command = {
            \ 'types': {
                \ 1: ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others'],
                \ 2: ['.hg', 'hg --cwd %s locate -I .'],
            \ },
            \ 'fallback': 'ag %s --nocolor -l -g ""'
        \ }

    " }}}
    " Git Gutter {{{

        hi SignColumn ctermbg=none

    " }}}
    " Gundo {{{

        let g:gundo_preview_bottom = 1

    " }}}
    " HTML5 {{{

        let g:event_handler_attributes_complete = 0
        let g:rdfa_attributes_complete = 0
        let g:microdata_attributes_complete = 0
        let g:atia_attributes_complete = 0

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
        let g:syntastic_quiet_messages = {'level': 'warnings'}

        " Do not validate the following file types.
        let g:syntastic_disabled_filetypes = ['html', 'python']

        " Set the display format.
        let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'

    " }}}
    " Tagbar {{{

        let g:tagbar_sort = 1            " Sort by name.
        let g:tagbar_compact = 0         " Use compact layout.
        let g:tagbar_expand = 1          " Expand window in GUI mode.

        " Define custom Zsh support (requires definition in ~/.ctags).
        let g:tagbar_type_zsh = {
            \ 'ctagstype': 'zsh',
            \ 'kinds': [
                \ 'f:functions:1'
            \ ],
            \ 'fold': 0
        \ }

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
            call UltiSnips#ExpandSnippet()
            return g:ulti_expand_res
        endfunction

        let g:ulti_jump_forwards_res = 0
        function! UltiSnips_MaybeJumpForwards()
            call UltiSnips#JumpForwards()
            return g:ulti_jump_forwards_res
        endfunction

        let g:ulti_jump_backwards_res = 0
        function! UltiSnips_MaybeJumpBackwards()
            call UltiSnips#JumpBackwards()
            return g:ulti_jump_backwards_res
        endfunction

        function! g:Wolfjourn_HandleCR()
            " For now, fuck this shit.
            "
            " if UltiSnips_MaybeExpandSnippet()
            "     return ""
            " elseif pumvisible()
            "     " Close completion menu
            "     " Use feedkeys since we need mappings
            "     " call feedkeys("\<Plug>ToggleYCM\<C-y>\<Plug>ToggleYCM")
            "     " return ""
            "     return "\<C-y>"
            " elseif delimitMate#WithinEmptyPair()
            if delimitMate#WithinEmptyPair()
                return delimitMate#ExpandReturn()
            else
                return "\<CR>"
            endif
        endfunction

        function! g:Wolfjourn_HandleTab()
            " if pumvisible()
            "     return "\<C-n>"
            " elseif UltiSnips_MaybeJumpForwards()
            "     return ""
            " elseif delimitMate#ShouldJump()
            if delimitMate#ShouldJump()
                return delimitMate#JumpAny()
            else
                return "\<Tab>"
            endif
        endfunction

        " function! g:Wolfjourn_HandleShiftTab()
        "     if pumvisible()
        "         return "\<C-p>"
        "     elseif UltiSnips_MaybeJumpBackwards()
        "         return ""
        "     else
        "         return "\<S-Tab>"
        "     endif
        " endfunction

        inoremap <CR> <C-R>=g:Wolfjourn_HandleCR()<CR>
        inoremap <Tab> <C-R>=g:Wolfjourn_HandleTab()<CR>
        " inoremap <S-Tab> <C-R>=g:Wolfjourn_HandleShiftTab()<CR>

        snoremap <Tab> <Esc>:call UltiSnips#JumpForwards()<CR>
        snoremap <S-Tab> <Esc>:call UltiSnips#JumpBackwards()<CR>

    " }}}
    " Yankring {{{

        " Hide the history file.
        let g:yankring_history_file = '.yankring-history'

    " }}}

" }}}
" Key Remapping ----------------------------------------------------------- {{{

    " "Focus" the current line.  Basically:
    "
    " 1. Close all folds.
    " 2. Open just the folds containing the current line.
    " 3. Move the line to a little bit (15 lines) above the center of the screen.
    "
    " This mapping wipes out the z mark, which I never use.
    nnoremap <Leader>z mzzMzvzz15<c-e>`z

    nnoremap <Leader>a :Ack!
    nnoremap <Leader>b :CtrlPBuffer<CR>
    nnoremap <Leader>c :CtrlPClearCache<CR>
    nnoremap <Leader>g :Gstatus<CR>
    nnoremap <Leader>i :TagbarToggle<CR>
    nnoremap <Leader>m :CtrlPMRU<CR>
    nnoremap <Leader>q :wq<CR>
    nnoremap <Leader>s :w<CR>
    nnoremap <Leader>u :GundoToggle<CR>

    " Faster substitute.
    nnoremap <Leader>S :%s//<left>

    " Easier linewise reselection.
    nnoremap <Leader>v V`]

    " Reselect pasted text.
    nnoremap <Leader>v V`]

    " Strip trailing whitespace.
    nnoremap <Leader>w :call StripTrailingWhitespace()<CR>

    " Disable search match highlight.
    nnoremap <Leader><space> :noh<CR>

    " Text Movement
     noremap <Leader>j :m+<CR>
     noremap <Leader>k :m-2<CR>
    vnoremap <Leader>j :m'>+<CR>gv
    vnoremap <Leader>k :m-2<CR>gv

    " edit/source vimrc
    " nnoremap <leader>ev :vsp $MYVIMRC<CR>
    nnoremap <leader>v :source $MYVIMRC<CR>

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

        nnoremap <C-h> <C-w>h<C-w>_
        nnoremap <C-j> <C-w>j<C-w>_
        nnoremap <C-k> <C-w>k<C-w>_
        nnoremap <C-l> <C-w>l<C-w>_

        " Horizontal split then move to bottom window.
        nnoremap <Leader>- <C-w>s<C-w>_

        " Vertical split then move to right window.
        nnoremap <Leader>\| <C-w>v<C-w>l<C-w>_

    " }}}
    " Text Alignment {{{

        nnoremap <Leader>Al :left<CR>
        nnoremap <Leader>Ac :center<CR>
        nnoremap <Leader>Ar :right<CR>
        vnoremap <Leader>Al :left<CR>
        vnoremap <Leader>Ac :center<CR>
        vnoremap <Leader>Ar :right<CR>

    " }}}
    " Miscellaneous Mappings {{{

        " Duplicate a selection.
        vnoremap D y'>p

        " Tab to indent in visual mode.
        " vnoremap <Tab> >gv

        " Shift+Tab to unindent in visual mode.
        " vnoremap <S-Tab> <gv

        " Re hard wrap paragraph.
        " nnoremap <Leader>q gqip

        " Reselect text ater indent/unindent.
        vnoremap < <gv
        vnoremap > >gv

        " Display-wise up/down movement instead of linewise.
        noremap j gj
        noremap k gk

        " Faster ESC.
        " inoremap jk <ESC>
        " inoremap kj <ESC>
        " inoremap jj <ESC>
        " inoremap kk <ESC>

        " Make Ctrl-C trigger InsertLeave autocmds
        "
        " This is very heavy-handed, but Ctrl-C is what I'm accustomed to using to
        " leave insert mode.
        inoremap <C-C> <Esc>`^

        " Yank to end of line
        nnoremap Y y$

        " Format Paragraph.
        " nnoremap <Leader>q gwap

        " Formatting, TextMate-style.
        nnoremap Q gqip

        " Change Case.
        nnoremap <C-u> g~iw
        inoremap <C-u> <ESC>eg~iwea

        " Write with sudo.
        cnoremap w!! w !sudo tee % >/dev/null

        " Toggle spell checking.
        " nnoremap <silent><Leader>s :set spell!<CR>

        " Shift+P replace selection without overwriting default register in vmode.
        vnoremap P p :call setreg('"', getreg('0'))<CR>

        " Quick return.
        inoremap <C-CR> <ESC>A<CR>
        inoremap <S-C-CR> <ESC>A:<CR>

        " Fix linewise visual selection of various text objects.
        nnoremap VV V
        nnoremap Vit vitVkoj
        nnoremap Vat vatV
        nnoremap Vab vabV
        nnoremap VaB vaBV

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
            if !&binary && &modifiable && &filetype != 'diff' && &ft != 'jade'
                let l:winview = winsaveview()
                %s/\s\+$//e
                let @/=''
                call winrestview(l:winview)
            endif
        endfunction

    " }}}

" }}}
