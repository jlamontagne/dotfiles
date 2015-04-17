" References:
"
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

" Plugins ----------------------------------------------------------------- {{{
    set nocompatible
    filetype off
    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()

    Plugin 'gmarik/Vundle.vim'
    Plugin 'godlygeek/tabular'
    Plugin 'sjl/gundo.vim'
    Plugin 'Lokaltog/vim-easymotion'
    " s?? to jump to next occurrence of ?? in the line
    " Disabled due to unfortunate collision with s/S
    " Plugin 'goldfeld/vim-seek'
    Plugin 'kien/ctrlp.vim'

    " ic, ac, iC, aC
    Plugin 'coderifous/textobj-word-column.vim'
    Plugin 'szw/vim-tags'
    let g:vim_tags_use_vim_dispatch = 1

    Plugin 'tpope/vim-vividchalk'
    " Plugin 'tpope/vim-sleuth'
    Plugin 'tpope/vim-repeat'
    " Provides pairs of bracket mappings for buffer, file navigation and editing.
    Plugin 'tpope/vim-unimpaired'
    Plugin 'tpope/vim-scriptease'
    " :Unlink/:Remove/:Move/etc
    Plugin 'tpope/vim-eunuch'
    " netrw via -
    Plugin 'tpope/vim-vinegar'
    " Easily search for, substitute, and abbreviate multiple variants of a word.
    " Also, coerce text: crs(nake)/crm(ixed)/cru(pper)/crc(amel)
    Plugin 'tpope/vim-abolish'
    " Continuously updated session files
    Plugin 'tpope/vim-obsession'
    " Plugin 'tpope/vim-tbone'
    " Plugin 'tpope/vim-jdaddy'
    Plugin 'tpope/vim-markdown'
    Plugin 'tpope/vim-rails'
    " Yes. Setup tmux to make this awesome. TODO
    Plugin 'tpope/vim-dispatch'
    Plugin 'tpope/vim-surround'
    Plugin 'tpope/vim-endwise'
    Plugin 'tpope/vim-git'
    Plugin 'tpope/vim-fugitive'
    Plugin 'tpope/vim-projectionist'
    Plugin 'altercation/vim-colors-solarized'
    Plugin 'w0ng/vim-hybrid'
    Plugin 'chriskempson/base16-vim'

    Plugin 'syntaxhaskell.vim'
    Plugin 'indenthaskell.vim'

    " Context aware paste (indentation)
    Plugin 'sickill/vim-pasta'
    Plugin 'mileszs/ack.vim'
    Plugin 'scrooloose/syntastic'
    Plugin 'tomtom/tcomment_vim'

    Plugin 'Raimondi/delimitMate'
    " Automatically closes functions, blocks, etc.
    Plugin 'kana/vim-textobj-user'
    Plugin 'michaeljsmith/vim-indent-object'
    " Configures % to match more than just single characters.
    Plugin 'vim-scripts/matchit.zip'
    " Disabled until I get around to properly integrating/learning this.
    " Plugin 'SirVer/ultisnips'
    " Plugin 'honza/vim-snippets'

    Plugin 'int3/vim-extradite'

    " Plugin 'rstacruz/sparkup'
    " Plugin 'othree/html5.vim'
    " Plugin 'amirh/HTML-AutoCloseTag'
    Plugin 'jelera/vim-javascript-syntax'
    Plugin 'pangloss/vim-javascript'
    Plugin 'mustache/vim-mustache-handlebars'
    " <c-y>,
    Plugin 'mattn/emmet-vim'

    call vundle#end()
    filetype plugin indent on

" }}}
" General Settings -------------------------------------------------------- {{{

    set ttimeoutlen=50
    set modelines=0
    set undolevels=1000
    set history=50
    set encoding=utf8
    " Share the clipboard.
    " set clipboard+=unnamed
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
    set winheight=5
    set winminheight=5
    set shortmess=aIoOtT
    set showmode
    set showcmd
    " Scroll n lines before vertical edge.
    set scrolloff=3
    " Scroll n lines before horizontal edge.
    set sidescroll=3
    set visualbell
    " Don't keep windows at equal size.
    " set noequalalways
    set splitbelow
    set splitright
    " Line break at the characters in breakat.
    set linebreak
    " Show ↪ at the beginning of wrapped lines.
    let &showbreak=nr2char(8618).' '
    set listchars=tab:▸\ ,trail:·
    " let &listchars = "tab:\u21e5\u00b7,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u26ad"
    " let &fillchars = "vert:\u259a,fold:\u00b7"
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
    " set relativenumber
    " set numberwidth=2
    set hidden
    set showmatch
    " Match for 3 tenths of a second.
    set matchtime=3
    " Pairs to match.
    set matchpairs+=<:>
    set printoptions+=syntax:y
    set printoptions+=number:y
    set cf
    " Set diff fill char.
    set fillchars+=diff:⣿
    set titlestring=%f\ "%h%m%r%w\ -\ %{v:progname}\ -\ %{substitute(getcwd(),\ $HOME,\ '~',\ '')}
    if has('mouse')
        set mouse=a
    endif

    " Default cinoptions are:
    " set cinoptions=>s,e0,n0,f0,{0,}0,^0,L-1,:s,=s,l0,b0,gs,hs,N0,ps,ts,is,+s, c3,C0,/0,(2s,us,U0,w0,W0,k0,m0,j0,J0,)20,*70,#0

    " Line up args on additional lines, etc.
    set cinoptions+=(0,w1,m1

    syntax on
    " set background=dark

    let g:hybrid_use_Xresources = 1
    " colorscheme hybrid

    " Change base16 scheme by changing xresources file
    colorscheme base16-default

    " colorscheme badwolf
    " colorscheme grb256
    " colorscheme vividchalk
    " colorscheme solarized

    " hi LineNr ctermbg=0 ctermfg=8 cterm=none
    " hi CursorLine cterm=none

" }}}
" Status Line ------------------------------------------------------------- {{{

    " Always show status.
    " set laststatus=2
    " Disable status line fill chars.
    " set fillchars+=stl:\ ,stlnc:\ " Space.
    " set statusline=%<%.99f\ %h%w%m%r%{SL('CapsLockStatusline')}%y%{SL('fugitive#statusline')}%*%=%-14.(%l,%c%V%)\ %P
    " set statusline=%<%f\ (%{&ft})%r\ %-4(%m%)%=%-16(%3l,%02c%03V%)\ %P

    set laststatus=2                                         " always show status
    set statusline=%<%f\                                     " Filename
    set statusline+=%w%h%m%r                                 " Options
    set statusline+=\ %y                                     " filetype
    set statusline+=\ [%{join(split(getcwd(),'/')[3:],'/')}] " current dir
    set statusline+=%=%-14.(%l,%c%V%)\ %p%%        " Right aligned file nav info

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

    set expandtab
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
    " Highlight VCS conflict markers.
    match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" }}}
" Folding ----------------------------------------------------------------- {{{

    set foldenable
    " syntax method slows ruby files to a crawl
    set foldmethod=marker
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

    " set wildignore+=.hg,.git,.svn
    set wildignore+=node_modules
    " set wildignore+=*.aux,*.out,*.toc
    " set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
    " set wildignore+=*.luac
    " set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest
    set wildignore+=*.pyc
    " set wildignore+=*.spl
    " set wildignore+=*~,#*#,*.sw?,%*,*=
    set wildignore+=*.DS_Store
    set wildignore+=tags

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

        " Use <Leader>S to sort properties.
        au BufNewFile,BufRead *.css,*.less
            \ nnoremap <buffer> <LocalLeader>S ?{<CR>jV/\v^\s*\}?$<CR>k:sort<CR>:noh<CR>

        " Jump to the last known position when reopening a file.
        au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal! g'\"" |
        \ endif

        " au FileType handlebars compiler html
        au BufNewFile,BufRead bash-fc-* setlocal filetype=sh
        " au BufNewFile,BufRead *.less setlocal filetype=less
        au BufNewFile,BufRead *.less setlocal filetype=css
        " au BufNewFile,BufRead *.handlebars,*.hbs setlocal filetype=html syntax=html
        au BufNewFile,BufRead *.js.es6 setlocal filetype=javascript
        au BufNewFile,BufRead *.m*down setlocal filetype=markdown
        au BufNewFile,BufRead Vagrantfile set filetype=ruby
        au BufNewFile,BufRead zshecl*,prompt_*_setup setlocal filetype=zsh

        " au FileType html compiler html
        " au FileType html ru ftplugin/html_autoclosetag.vim
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

" }}}
" Plugin Settings --------------------------------------------------------- {{{

    let g:delimitMate_expand_cr = 2
    let g:delimitMate_expand_space = 1
    " let g:delimitMate_excluded_ft = "html.mustache,html.handlebars,html,xhtml"

    " Don't jump over shit on other lines
    let g:delimitMate_jump_expansion = 0

    " Ack/The Silver Searcher {{{
        let g:ackprg = 'ag --nogroup --nocolor --column'
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
    " Gundo {{{

        let g:gundo_preview_bottom = 1

    " }}}
    " Surround {{{

        let g:surround_40 = "(\r)"
        let g:surround_91 = "[\r]"
        let g:surround_60 = "<\r>"

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

        function! g:InsertCRWrapper()
            " if UltiSnips_MaybeExpandSnippet()
            "     return ""
            if pumvisible()
                return "\<C-y>"
            "     " Close completion menu
            "     " Use feedkeys since we need mappings
            "     " call feedkeys("\<Plug>ToggleYCM\<C-y>\<Plug>ToggleYCM")
            "     " return ""
            elseif delimitMate#WithinEmptyPair()
                return delimitMate#ExpandReturn()
            else
                return "\<CR>"
            endif
        endfunction

        function! g:InsertTabWrapper()
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

        inoremap <CR> <C-R>=g:InsertCRWrapper()<CR>
        inoremap <Tab> <C-R>=g:InsertTabWrapper()<CR>
        " inoremap <S-Tab> <C-R>=g:Wolfjourn_HandleShiftTab()<CR>
        " let g:user_emmet_next_key = '<s-tab>'

        snoremap <Tab> <Esc>:call UltiSnips#JumpForwards()<CR>
        " snoremap <S-Tab> <Esc>:call UltiSnips#JumpBackwards()<CR>

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

    nnoremap <silent> <leader>ps :exe ":profile start profile.log"<cr>:exe ":profile func *"<cr>:exe ":profile file *"<cr>
    nnoremap <silent> <leader>pe :exe ":profile pause"<cr>:noautocmd qall!<cr>

    nnoremap <Leader>a :Ack!
    nnoremap <Leader>b :CtrlPBuffer<CR>
    nnoremap <Leader>c :CtrlPClearCache<CR>
    nnoremap <Leader>g :Gstatus<CR>
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
        inoremap jj <ESC>
        " inoremap kk <ESC>
        " aug auto_leave_insert
        "     au!
        "     au CursorHoldI * stopinsert
        "     au InsertEnter * let updaterestore=&updatetime | set updatetime=5000
        "     au InsertLeave * let &updatetime=updaterestore
        " aug end

        " Make Ctrl-C trigger InsertLeave autocmds
        "
        " This is very heavy-handed, but Ctrl-C is what I'm accustomed to using to
        " leave insert mode.
        inoremap <C-C> <Esc>`^
        " inoremap <C-C> <Esc>

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

        " Diff.
        nnoremap <silent><Leader>do :diffoff!<CR>
        nnoremap <silent><Leader>dg :diffget<CR>:diffupdate<CR>
        nnoremap <silent><Leader>dp :diffput<CR>:diffupdate<CR>
        nnoremap <silent><Leader>du :diffupdate<CR>

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

    function! SL(function)
        if exists('*'.a:function)
            return call(a:function,[])
        else
            return ''
        endif
    endfunction

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
