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
set hidden
" disable line wrapping
set textwidth=0

set backup
silent execute '!mkdir -p "'.$HOME.'/.vim/tmp"'
silent execute '!rm -f '.$HOME.'/.vim/tmp/*~'
set backupdir=$HOME/.vim/tmp/
set directory=$HOME/.vim/tmp/

set nocompatible               " Be iMproved
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'
Bundle 'vividchalk.vim'
Bundle 'tpope/vim-fugitive'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'jelera/vim-javascript-syntax'
Bundle 'ack.vim'
Bundle 'unite.vim'
" Remember to run make -f make_unix.mak
Bundle 'Shougo/vimproc'
Bundle 'groenewege/vim-less'
Bundle 'scrooloosesyntastic'
Bundle 'surround.vim'
Bundle 'nelstrom/vim-qargs'
Bundle 'visualstar.vim'
Bundle 'sunesimonsen/vim-unite-repo-files'
Bundle 'YankRing.vim'

if has('python')
    Bundle 'UltiSnips'

    let g:UltiSnipsSnippetDirectories=['snippets']

    let g:UltiSnipsExpandTrigger="<tab>"
    let g:UltiSnipsJumpForwardTrigger="<tab>"
    let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

    autocmd BufNewFile,BufRead *.snippets set filetype=snippets
    autocmd BufNewFile,BufRead *.js :UltiSnipsAddFiletypes javascript
    autocmd BufNewFile,BufRead *.spec.js :UltiSnipsAddFiletypes mocha
    autocmd BufNewFile,BufRead *.ko :UltiSnipsAddFiletypes knockout
endif

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

map <C-Space> ?
map <C-@> <C-Space>
map <space> /

map <silent> <leader><CR> :set hlsearch! hlsearch?<CR>

set ignorecase          " case-insensitive search

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

nnoremap <Leader>ar :.,$s/\<<C-r><C-w>\>//gc<Left><Left><Left>
vmap <Leader>as <C-v>_o$A
vmap <Leader>ap <C-v>_o$I

" Unite
nnoremap <leader>p :<C-u>Unite -no-split -buffer-name=files -start-insert repo_files<cr>
nnoremap <leader>e :<C-u>Unite -no-split -buffer-name=files -start-insert file<cr>
nnoremap <leader>r :<C-u>Unite -no-split -buffer-name=mru -start-insert file_mru<cr>
nnoremap <leader>b :<C-u>Unite -no-split -buffer-name=buffer -start-insert buffer<cr>
nnoremap <leader>c :<C-u>Unite -no-split -buffer-name=commands -start-insert command<cr>
nnoremap <leader>y :<C-u>Unite -no-split -buffer-name=yank history/yank<cr>
nnoremap <leader>fb :<C-u>Unite -no-split -buffer-name=bookmarks -start-insert bookmark<cr>

call unite#custom_source('repo_files', 'ignore_pattern', 'translation-jobs/\|calendar-frontend/\|3rdparty/\|calendar/\|debian/\|\.jpg$\|\.png')

call unite#filters#matcher_default#use(['matcher_fuzzy'])

let g:unite_source_repo_files_rule = {
    \   'git' : {
    \   'located' : '.git',
    \   'command' : 'git',
    \   'exec' : '%c ls-files --cached --others --exclude-standard',
    \ } }

let g:unite_source_history_yank_enable = 1
let g:unite_source_rec_async_command = 'ack -f --nofilter'

" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  " Enable navigation with control-j and control-k in insert mode
  imap <buffer> <C-j> <Plug>(unite_select_next_line)
  imap <buffer> <C-k> <Plug>(unite_select_previous_line)
  map <buffer> <esc> <Plug>(unite_exit)
endfunction

" Menu maps
let g:unite_source_menu_menus = {}
"nnoremap <silent>mg :Unite -silent -start-insert menu:git<CR>

"fugitive
autocmd BufReadPost fugitive://* set bufhidden=delete
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

" Fast editing of the .vimrc
map <leader>so :so ~/.vimrc<CR>

set splitbelow
set splitright

let g:syntastic_enable_balloons = 1
let g:syntastic_enable_highlighting = 1
let g:syntastic_enable_signs = 1
let g:syntastic_echo_current_error = 1

autocmd BufRead,BufNewFile *.ko set filetype=html

" Next and previous
nnoremap øc :cnext<cr>
nnoremap Øc :clast<cr>
nnoremap åc :cprev<cr>
nnoremap Åc :cfirst<cr>

nnoremap øl :lnext<cr>
nnoremap Øl :llast<cr>
nnoremap ål :lprev<cr>
nnoremap Ål :lfirst<cr>
