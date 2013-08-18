" requires
" sudo apt-get install ack-grep
" sudo ln -s /usr/bin/ack-grep /usr/local/bin/ack
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
Bundle 'vividchalk.vim'
Bundle 'gmarik/vundle'
Bundle 'tpope/vim-fugitive'
Bundle 'Lokaltog/vim-easymotion'
Bundle "pangloss/vim-javascript"
Bundle 'snipMate'
Bundle 'ack.vim'
Bundle 'walm/jshint.vim'
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'ScmFrontEnd-former-name--MinSCM'

" Hide the toolbar
:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar

set incsearch		" do incremental searching

" Set 4 lines to the curors - when moving vertical..
set scrolloff=4

colorscheme vividchalk

" Colors
set t_Co=256

set gfn=DejaVu\ Sans\ Mono\ 12

set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar

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
map <leader>e :FufFileWithCurrentBufferDir<CR>
map <leader>b :FufBuffer<CR>
map <leader>p :MinSCMFindFile<CR>

" Fast editing of the .vimrc
map <leader>rc :e ~/.vimrc<CR>
map <leader>so :so ~/.vimrc<CR>

set splitbelow
set splitright

" Javascript
" autocmd BufWritePost *.js JSHint
