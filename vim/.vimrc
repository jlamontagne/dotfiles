" References:
"
" https://github.com/skwp/dotfiles
" http://statico.github.io/vim2.html
" http://dougblack.io/words/a-good-vimrc.html
" http://stackoverflow.com/questions/19030463/how-do-you-work-with-large-projects-in-vim
" https://github.com/spf13/spf13-vim
" https://github.com/tpope/tpope/blob/master/.vimrc
" https://github.com/garybernhardt/dotfiles/blob/master/.vimrc
" http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html

" TODO: Unite as a replacement for ctrlp/vim-vinegar/etc.
" http://www.codeography.com/2013/06/17/replacing-all-the-things-with-unite-vim.html
" http://bling.github.io/blog/2013/06/02/unite-dot-vim-the-plugin-you-didnt-know-you-need/

" fish screws things up
set shell=sh

set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'sheerun/vim-polyglot'
Plugin 'gregsexton/gitv'
Plugin 'vim-scripts/matchit.zip'
Plugin 'AndrewRadev/splitjoin.vim'
Plugin 'tpope/vim-ragtag'
Plugin 'airblade/vim-gitgutter'
Plugin 'dag/vim-fish'

" Text objects
Plugin 'austintaylor/vim-indentobject'
Plugin 'coderifous/textobj-word-column.vim'
Plugin 'kana/vim-textobj-datetime'
Plugin 'kana/vim-textobj-entire'
Plugin 'kana/vim-textobj-function'
Plugin 'kana/vim-textobj-user'
Plugin 'lucapette/vim-textobj-underscore'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'thinca/vim-textobj-function-javascript'
Plugin 'vim-scripts/argtextobj.vim'

Plugin 'godlygeek/tabular'
Plugin 'sjl/gundo.vim'
Plugin 'kien/ctrlp.vim'

Plugin 'tpope/vim-sleuth'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-vinegar'
" Easily search for, substitute, and abbreviate multiple variants of a word.
" Also, coerce text: crs(nake)/crm(ixed)/cru(pper)/crc(amel)
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-git'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-projectionist'
" Plugin 'tpope/vim-vividchalk'
" Plugin 'altercation/vim-colors-solarized'
" Plugin 'w0ng/vim-hybrid'
Plugin 'chriskempson/base16-vim'

" Context aware paste (indentation)
Plugin 'sickill/vim-pasta'
Plugin 'mileszs/ack.vim'
Plugin 'scrooloose/syntastic'
Plugin 'tomtom/tcomment_vim'

" Automatically closes functions, blocks, etc.
Plugin 'Raimondi/delimitMate'
Plugin 'michaeljsmith/vim-indent-object'
Plugin 'jlamontagne/ultisnips'
" Plugin 'honza/vim-snippets'
Plugin 'int3/vim-extradite'
" <c-y>,
Plugin 'mattn/emmet-vim'

call vundle#end()
filetype plugin indent on

set ttimeoutlen=50
set modelines=0
set undolevels=1000
set history=50
set encoding=utf8
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
let mapleader = ','
let maplocalleader = ';'
set winheight=5
set winminheight=5
set shortmess=aIoOtT
set showmode
set showcmd
set scrolloff=3
set sidescroll=3
set visualbell
set linebreak
" Show ↪ at the beginning of wrapped lines.
let &showbreak=nr2char(8618).' '
set listchars=tab:▸\ ,trail:·
set list
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
set numberwidth=2
set hidden
set showmatch
" Match for 3 tenths of a second.
set matchtime=3
set cf
" Set diff fill char.
set fillchars+=diff:⣿
set titlestring=%f\ "%h%m%r%w\ -\ %{v:progname}\ -\ %{substitute(getcwd(),\ $HOME,\ '~',\ '')}

" Line up args on additional lines, etc.
set cinoptions+=(0,w1,m1

syntax on
set background=dark

if filereadable(expand("~/.vimrc_background"))
    let base16colorspace=256
    source ~/.vimrc_background
endif

hi Search ctermfg=0 ctermbg=9
hi IncSearch ctermfg=0 ctermbg=3

" let g:hybrid_use_Xresources = 1
" colorscheme hybrid
" colorscheme badwolf
" colorscheme grb256
" colorscheme vividchalk

" Always show status.
set laststatus=2                                         " always show status
set statusline=%<%f\                                     " Filename
set statusline+=%w%h%m%r                                 " Options
set statusline+=\ %y                                     " filetype
set statusline+=\ [%{join(split(getcwd(),'/')[3:],'/')}] " current dir
set statusline+=%=%-14.(%l,%c%V%)\ %p%%        " Right aligned file nav info

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

" set expandtab
" set shiftwidth=4
" set softtabstop=4
" Do not select the end of line.
" XXX: Using 'old' breaks UltiSnips by selecting one character past the
" placeholder.
"
" set selection=old

" Don't shift to the next round tab stop. This fucks with reindenting
" manually indented code. e.g.:
"
" <div class="lined up"
"      id="nicely">
"
" set shiftround

" Copy indent from the current line.
set autoindent
set wrap
set textwidth=80
set colorcolumn=+1
set formatoptions=tcqrn1
set virtualedit+=block

set foldenable
set foldmethod=marker
set foldnestmax=2
set foldlevelstart=5

" space open/closes fold
nnoremap <space> za

" Make zO recursively open the top level fold regardless of cursor placement.
nnoremap zO zCzO

highlight Folded cterm=NONE ctermfg=10 ctermbg=0

" Show a list entries.
set wildmenu
set wildmode=list:longest,full
set wildignore+=.hg,.git,.svn
set wildignore+=node_modules
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.luac
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest
set wildignore+=*.pyc
set wildignore+=*.DS_Store
set wildignore+=tags

" Resize splits when the window is resized.
au VimResized * exe "normal! \<c-w>="

aug file_settings
    au!

    " Jump to the last known position when reopening a file.
    au BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal! g'\"" |
    \ endif

    au BufNewFile,BufRead bash-fc-* setlocal filetype=sh
    " au BufNewFile,BufRead *.less setlocal filetype=less
    au BufNewFile,BufRead *.less setlocal filetype=css
    " au BufNewFile,BufRead *.handlebars,*.hbs setlocal filetype=html syntax=html
    au BufNewFile,BufRead *.js.es6 setlocal filetype=javascript
    au BufNewFile,BufRead *.m*down setlocal filetype=markdown
    au BufNewFile,BufRead Vagrantfile set filetype=ruby
    au BufNewFile,BufRead zshecl*,prompt_*_setup setlocal filetype=zsh

    au Filetype qf setlocal colorcolumn=0 nolist nocursorline nowrap
    au FileType git* setlocal noexpandtab tabstop=4 softtabstop=4 shiftwidth=4 nofoldenable textwidth=72
    au FileType text,markdown,eruby setlocal wrap linebreak
    au FileType html.handlebars,html,xhtml setlocal wrap linebreak et sw=2 sts=2 indentkeys-={,}
    au FileType vim setlocal foldmethod=marker
    au FileType help nnoremap <silent><buffer> q :q<CR>
    au FileType gitcommit setlocal spell
    au FileType javascript setlocal et sw=2 sts=2
    autocmd FileType * if exists("+omnifunc") && &omnifunc == "" | setlocal omnifunc=syntaxcomplete#Complete | endif
    autocmd FileType * if exists("+completefunc") && &completefunc == "" | setlocal completefunc=syntaxcomplete#Complete | endif
aug end

let g:syntastic_html_tidy_exec = 'tidy'
let g:syntastic_html_tidy_ignore_errors = [" proprietary attribute \"ng-"]

let g:delimitMate_expand_cr = 2
let g:delimitMate_expand_space = 1
let g:delimitMate_excluded_ft = "scss,html.mustache,html.handlebars,html,xhtml"

" Don't jump over shit on other lines
let g:delimitMate_jump_expansion = 0

let g:ackprg = 'ag --nogroup --nocolor --column'

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

let g:gundo_preview_bottom = 1

let g:surround_40 = "(\r)"
let g:surround_91 = "[\r]"
let g:surround_60 = "<\r>"

" Map 'gcp' comment the current paragraph (block).
if exists('loaded_tcomment')
    nmap <silent> gcp <c-_>p
endif

let g:UltiSnipsSnippetsDir = "~/.vim/UltiSnips"
let g:UltiSnipsExpandTrigger = "<Nul>"
let g:UltiSnipsJumpForwardTrigger = "<Nul>"

let g:ulti_expand_res = 0
function! MaybeExpandSnippet()
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

function! g:InsertCRWrapper()
    let snippet = MaybeExpandSnippet()

    if pumvisible()
        return "\<C-y>"
        " Close completion menu
        " Use feedkeys since we need mappings
        " call feedkeys("\<Plug>ToggleYCM\<C-y>\<Plug>ToggleYCM")
        " return ""
    elseif delimitMate#WithinEmptyPair()
        return delimitMate#ExpandReturn()
    elseif !snippet
        return "\<CR>"
    else
        return ""
    endif
endfunction

function! g:InsertTabWrapper()
    if pumvisible()
        return "\<C-y>"
    elseif UltiSnips_MaybeJumpForwards()
        return ""
    elseif delimitMate#ShouldJump()
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

function! g:InsertSpaceWrapper()
    if pumvisible()
        return "\<C-y>"
        " Close completion menu
        " Use feedkeys since we need mappings
        " call feedkeys("\<Plug>ToggleYCM\<C-y>\<Plug>ToggleYCM")
        " return ""
    else
        return "\<Space>"
    endif
endfunction

inoremap <CR> <C-R>=g:InsertCRWrapper()<CR>
inoremap <Tab> <C-R>=g:InsertTabWrapper()<CR>
" inoremap <S-Tab> <C-R>=g:Wolfjourn_HandleShiftTab()<CR>
" let g:user_emmet_next_key = '<s-tab>'
inoremap <Space> <C-R>=g:InsertSpaceWrapper()<CR>

snoremap <Tab> <Esc>:call UltiSnips#JumpForwards()<CR>
" snoremap <S-Tab> <Esc>:call UltiSnips#JumpBackwards()<CR>

nnoremap <silent> <leader>ps :exe ":profile start profile.log"<cr>:exe ":profile func *"<cr>:exe ":profile file *"<cr>
nnoremap <silent> <leader>pe :exe ":profile pause"<cr>:noautocmd qall!<cr>

nnoremap <Leader>a :Ack!
nnoremap <Leader>b :CtrlPBuffer<CR>
nnoremap <Leader>g :Gstatus<CR>
nnoremap <Leader>m :CtrlPMRU<CR>
nnoremap <Leader>q :wq<CR>
nnoremap <Leader>s :w<CR>
nnoremap <Leader>u :GundoToggle<CR>

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
nnoremap <leader>v :so ~/.vimrc<CR>

" Tabs
map <Leader>tt :tabnew<CR>
map <Leader>tc :tabclose<CR>
noremap <Leader>tm :tabmove<CR>
noremap <Leader>tn :tabnext<CR>
noremap <Leader>tp :tabprevious<CR>

" Windows
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
" Horizontal split then move to bottom window.
nnoremap <Leader>- <C-w>s
" Vertical split then move to right window.
nnoremap <Leader>\| <C-w>v<C-w>l

" Display-wise up/down movement instead of linewise.
noremap j gj
noremap k gk

inoremap jj <ESC>

" Make Ctrl-C trigger InsertLeave autocmds
"
" This is very heavy-handed, but Ctrl-C is what I'm accustomed to using to
" leave insert mode.
inoremap <C-C> <Esc>`^

" Yank to end of line
nnoremap Y y$

" Formatting, TextMate-style.
nnoremap Q gqip

" Toggle paste.
set pastetoggle=<F12>

cabbr cdf cd %:p:h<CR>
cabbr lcdf lcd %:p:h<CR>
cabbr cwd lcd %:p:h

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

" Strip Trailing Whitespace
function! StripTrailingWhitespace()
    if !&binary && &modifiable && &filetype != 'diff' && &ft != 'jade'
        let l:winview = winsaveview()
        %s/\s\+$//e
        let @/=''
        call winrestview(l:winview)
    endif
endfunction
