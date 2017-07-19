" https://github.com/skwp/dotfiles
" http://statico.github.io/vim2.html
" http://dougblack.io/words/a-good-vimrc.html
" http://stackoverflow.com/questions/19030463/how-do-you-work-with-large-projects-in-vim
" https://github.com/spf13/spf13-vim
" https://github.com/tpope/tpope/blob/master/.vimrc
" https://github.com/garybernhardt/dotfiles/blob/master/.vimrc
" http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html
" http://www.codeography.com/2013/06/17/replacing-all-the-things-with-unite-vim.html
" http://bling.github.io/blog/2013/06/02/unite-dot-vim-the-plugin-you-didnt-know-you-need/

set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

" Language packs (syntax, index, ftplugin, ftdetect)
Plugin 'sheerun/vim-polyglot'

" Git integrations
Plugin 'tpope/vim-fugitive'

" File navigation
Plugin 'kien/ctrlp.vim'
Plugin 'tpope/vim-vinegar'

" Match indent settings to what's in a file
Plugin 'tpope/vim-sleuth'

" Make plugin commands repeatable
Plugin 'tpope/vim-repeat'

" Useful mappings from the man himself
Plugin 'tpope/vim-unimpaired'

" :Abolish and :Subvert
Plugin 'tpope/vim-abolish'

" Manage surroundings of strings
Plugin 'tpope/vim-surround'

" Auto add block endings
Plugin 'tpope/vim-endwise'

" Colours!
Plugin 'chriskempson/base16-vim'

" Global search
Plugin 'mileszs/ack.vim'

" Comment/uncomment
Plugin 'tpope/vim-commentary'

" Automatically closes functions, blocks, etc.
Plugin 'Raimondi/delimitMate'

" Medium distance text motion
Plugin 'justinmk/vim-sneak'

" Elixir alchemist
Plugin 'slashmili/alchemist.vim'

call vundle#end()
filetype plugin indent on

syntax on
set autoindent
set autoread
set autowrite
set background=dark
set backspace=indent,eol,start
set backup
set backupdir^=$HOME/.vim/backup//
set cf
set cinoptions+=(0,w1,m1
set colorcolumn=+1
set completeopt=menuone,preview
set dir^=$HOME/.vim/swap//
set encoding=utf8
set fillchars+=diff:⣿
set foldenable
set foldlevelstart=5
set foldmethod=marker
set foldnestmax=2
set formatoptions=tcqrn1
set hidden
set history=50
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set lazyredraw
set linebreak
set list
set listchars=tab:▸\ ,trail:·
set matchtime=3
set modelines=0
set numberwidth=2
set relativenumber
set ruler
set scrolloff=3
set shortmess=aIoOtT
set showcmd
set showmatch
set showmode
set sidescroll=3
set smartcase
set spelllang=en_ca
set titlestring=%f\ "%h%m%r%w\ -\ %{v:progname}\ -\ %{substitute(getcwd(),\ $HOME,\ '~',\ '')}
set ttimeoutlen=50
set ttyfast
set undodir^=$HOME/.vim/undo//
set undofile
set undolevels=1000
set undoreload=1000
set viewdir^=$HOME/.vim/view//
set virtualedit+=block
set visualbell
set wildmenu
set winheight=5
set winminheight=5
set wrap
set wrapscan
set ww+=<,>,[,]
let mapleader = ','
let maplocalleader = ';'

if filereadable(expand("~/.vimrc_background"))
    let base16colorspace=256
    source ~/.vimrc_background
endif

au VimResized * exe "normal! \<c-w>="

aug file_settings
    au!

    " Jump to the last known position when reopening a file.
    au BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal! g'\"" |
    \ endif

    au FileType help nnoremap <silent><buffer> q :q<CR>
    au FileType gitcommit setlocal spell
    au FileType javascript setlocal tw=80
    au FileType elixir setlocal tw=80
aug end

let g:ackprg = 'ag --nogroup --nocolor --column'
let g:delimitMate_expand_cr = 2
let g:delimitMate_expand_space = 1
let g:delimitMate_excluded_ft = "scss,html.mustache,html.handlebars,html,xhtml"
let g:delimitMate_jump_expansion = 0
let g:ctrlp_working_path_mode = 'raw'
let g:ctrlp_max_height = 10
let g:ctrlp_persistent_input = 0
let g:ctrlp_map = '<Leader>t'
let g:ctrlp_lazy_update = 1
let g:ctrlp_extensions = ['tag', 'buffertag', 'quickfix', 'dir']
let g:ctrlp_custom_ignore = {
    \ 'dir': '\.git$\|\.hg$\|\.svn$',
    \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$'
\ }
let g:ctrlp_user_command = {
    \ 'types': {
        \ 1: ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others'],
    \ },
    \ 'fallback': 'ag %s --nocolor -l -g ""'
\ }
let g:surround_40 = "(\r)"
let g:surround_91 = "[\r]"
let g:surround_60 = "<\r>"

function! g:InsertCRWrapper()
    if pumvisible()
        return "\<C-y>"
    elseif delimitMate#WithinEmptyPair()
        return delimitMate#ExpandReturn()
    else
        return "\<CR>"
    endif
endfunction

function! g:InsertTabWrapper()
    if pumvisible()
        return "\<C-y>"
    elseif delimitMate#ShouldJump()
        return delimitMate#JumpAny()
    else
        return "\<Tab>"
    endif
endfunction

function! g:InsertSpaceWrapper()
    if pumvisible()
        return "\<C-y>"
    else
        return "\<Space>"
    endif
endfunction

inoremap <CR> <C-R>=g:InsertCRWrapper()<CR>
inoremap <Tab> <C-R>=g:InsertTabWrapper()<CR>
inoremap <Space> <C-R>=g:InsertSpaceWrapper()<CR>
nnoremap / /\v
vnoremap / /\v
nnoremap ? ?\v
vnoremap ? ?\v
nnoremap <space> za
nnoremap <silent> <leader>ps :exe ":profile start profile.log"<cr>:exe ":profile func *"<cr>:exe ":profile file *"<cr>
nnoremap <silent> <leader>pe :exe ":profile pause"<cr>:noautocmd qall!<cr>
nnoremap <Leader>a :Ack!
nnoremap <Leader>b :CtrlPBuffer<CR>
nnoremap <Leader>g :Gstatus<CR>
nnoremap <Leader>m :CtrlPMRU<CR>
nnoremap <Leader>q :wq<CR>
nnoremap <Leader>s :w<CR>
nnoremap <Leader><space> :noh<CR>
 noremap <Leader>j :m+<CR>
 noremap <Leader>k :m-2<CR>
vnoremap <Leader>j :m'>+<CR>gv
vnoremap <Leader>k :m-2<CR>gv
nnoremap <leader>ev :vsp $MYVIMRC<CR>
nnoremap <leader>v :so ~/.vimrc<CR>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <Leader>- <C-w>s
nnoremap <Leader>\| <C-w>v<C-w>l
noremap j gj
noremap k gk
inoremap jj <ESC>
inoremap <C-C> <Esc>`^
nnoremap Y y$
nnoremap Q gqip

hi Search ctermfg=0 ctermbg=9
hi IncSearch ctermfg=0 ctermbg=3
