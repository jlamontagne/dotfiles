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

" fish screws things up
set shell=sh

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

" Project configuration
" Plugin 'tpope/vim-projectionist'

" Colours!
Plugin 'chriskempson/base16-vim'

" Global search
Plugin 'mileszs/ack.vim'

" Comment/uncomment
Plugin 'tpope/vim-commentary'
"Plugin 'tomtom/tcomment_vim'

" Automatically closes functions, blocks, etc.
Plugin 'Raimondi/delimitMate'

Plugin 'jlamontagne/ultisnips'
" Plugin 'honza/vim-snippets'

" Medium distance text motion
Plugin 'justinmk/vim-sneak'

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
set statusline=%<%f\
set statusline+=%w%h%m%r
set statusline+=\ %y
set statusline+=\ [%{join(split(getcwd(),'/')[3:],'/')}]
set statusline+=%=%-14.(%l,%c%V%)\ %p%%
set textwidth=80
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
set wildignore+=*.DS_Store
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.luac
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest
set wildignore+=*.pyc
set wildignore+=.hg,.git,.svn
set wildignore+=node_modules
set wildignore+=tags
set wildmenu
set wildmode=list:longest,full
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

hi Search ctermfg=0 ctermbg=9
hi IncSearch ctermfg=0 ctermbg=3

nnoremap / /\v
vnoremap / /\v
nnoremap ? ?\v
vnoremap ? ?\v
nnoremap <space> za

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
    au BufNewFile,BufRead *.js setlocal filetype=javascript
    au BufNewFile,BufRead *.m*down setlocal filetype=markdown
    au BufNewFile,BufRead Vagrantfile set filetype=ruby
    au BufNewFile,BufRead zshecl*,prompt_*_setup setlocal filetype=zsh

    au Filetype qf setlocal colorcolumn=0 nolist nocursorline nowrap
    au FileType git* setlocal noexpandtab tabstop=4 softtabstop=4 shiftwidth=4 nofoldenable textwidth=72
    au FileType text,markdown,eruby setlocal wrap linebreak
    au FileType html.handlebars,html,xhtml setlocal wrap linebreak et tw=0 sw=2 sts=2 indentkeys-={,}
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
    \ },
    \ 'fallback': 'ag %s --nocolor -l -g ""'
\ }

let g:surround_40 = "(\r)"
let g:surround_91 = "[\r]"
let g:surround_60 = "<\r>"

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
