" requires
" apt-get install ack-grep
" https://github.com/gmarik/vundle
set nocompatible               " be iMproved
filetype off

filetype plugin indent on
set tabstop=4
set shiftwidth=4
set expandtab

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'vibrantink'
Bundle 'gmarik/vundle'
Bundle 'tpope/vim-fugitive'
Bundle 'Lokaltog/vim-easymotion'
Bundle "pangloss/vim-javascript"
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'snipMate'
Bundle 'ack.vim'

set incsearch		" do incremental searching

" Set 4 lines to the curors - when moving vertical..
set scrolloff=4

colorscheme vibrantink

set gfn=DejaVu\ Sans\ Mono\ 12  

let mapleader = ","
let g:mapleader = ","

map æ :
map Æ @
map ½ $
imap ½ $
vmap ½ $
cmap ½ $

" Indent with tab in visual mode
vmap <S-Tab> <gv
vmap <Tab> >gv

map <space> /
map <c-space> ?

map <silent> <leader><CR> :set hlsearch! hlsearch?<CR>

set ignorecase          " case-insensitive search
set smartcase

" Dont use smartcase or ignorecase for * and #
nnoremap * /\<<C-R>=expand('<cword>')<CR>\><CR>
nnoremap # ?\<<C-R>=expand('<cword>')<CR>\><CR>

" Set the current directory to the buffer location
autocmd BufEnter * silent! lcd %:p:h

" Smart way to move btw. windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" FuzzyFinder
map <leader>b :FufBuffer<CR>
map <leader>e :FufFile<CR>
map <leader>d :FufDir<CR>

function! ProjectFuzzyFind()
    let origcurdir = getcwd()
    let curdir = origcurdir
    let prevdir = ""

    while curdir != prevdir
        if isdirectory(".git")
            break
        endif
        cd ..
        let prevdir = curdir
        let curdir = getcwd()
    endwhile

    if isdirectory(".git")
        let files = split(system('git ls-files'), '\n')
        echo files
        call fuf#givenfile#launch('', 0, '>', files)
    endif
    cd `=origcurdir`
endfunction

map <leader>p :call ProjectFuzzyFind()<CR>

" Wildmenu and Fuzzyfinder like dynamic menus
highlight Pmenu    guifg=white guibg=#808080
highlight PmenuSel guifg=black guibg=#ffbc29
highlight WildMenu guifg=black guibg=#ffbc29

" Fast editing of the .vimrc
map <leader>rc :e ~/.vimrc<CR>
map <leader>so :so ~/.vimrc<CR>
