set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

" Language packs (syntax, index, ftplugin, ftdetect)
Plugin 'sheerun/vim-polyglot'
" Plugin 'lambdatoast/elm.vim'
" Plugin 'elixir-editors/vim-elixir'

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

" Elixir alchemist
" K - ExDoc
" <C-]>/<C-T> - Jump
Plugin 'slashmili/alchemist.vim'

Plugin 'sjl/gundo.vim'

" <Leader>di, ds, b(ox), a(rrow), l(ine)
" Plugin 'vim-scripts/DrawIt'

call vundle#end()
filetype plugin indent on

syntax on
set autoread
set autowrite
set colorcolumn=+1
set hlsearch
set incsearch
set numberwidth=2
set relativenumber
set showmatch

let mapleader = ','
let maplocalleader = ';'

if filereadable(expand("~/.vimrc_background"))
    let base16colorspace=256
    source ~/.vimrc_background
endif

au VimResized * exe "normal! \<c-w>="

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

" let g:polyglot_disabled = ['elm', 'elixir']

function! g:InsertCRWrapper()
    if delimitMate#WithinEmptyPair()
        return delimitMate#ExpandReturn()
    else
        return "\<CR>"
    endif
endfunction

function! g:InsertTabWrapper()
    if delimitMate#ShouldJump()
        return delimitMate#JumpAny()
    else
        return "\<Tab>"
    endif
endfunction

inoremap <CR> <C-R>=g:InsertCRWrapper()<CR>
inoremap <Tab> <C-R>=g:InsertTabWrapper()<CR>

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
nnoremap <Leader>r :CtrlPMRU<CR>
nnoremap <Leader>q :wq<CR>
nnoremap <Leader>s :w<CR>
nnoremap <Leader>u :GundoToggle<CR>
nnoremap <Leader><space> :noh<CR>
 noremap <Leader>j :m+<CR>
 noremap <Leader>k :m-2<CR>
vnoremap <Leader>j :m'>+<CR>gv
vnoremap <Leader>k :m-2<CR>gv
nnoremap <leader>ev :vsp $MYVIMRC<CR>
nnoremap <leader>v :so $MYVIMRC<CR>
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
nnoremap <Leader>f gggqG

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
aug end

aug elixir
    au!

    au FileType elixir setlocal tw=80

    " ,m(ake) - Saves and compiles the code
    au FileType elixir nnoremap <Leader>m :compiler mix<CR>:make<CR>

    " ,test - Saves and tests the code
    au FileType elixir nnoremap <Leader>test :compiler exunit<CR>:make<CR>

    " Format Elixir files on save
    autocmd BufWritePost *.exs,*.ex silent! execute "!mix format %" | redraw!
aug end

hi Search ctermfg=0 ctermbg=9
hi IncSearch ctermfg=0 ctermbg=3
