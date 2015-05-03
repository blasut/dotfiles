"Not supporting Vi
set nocompatible
execute pathogen#infect('bundle/{}')

" Syntax stuff
filetype off
filetype plugin indent on
syntax on
set number

"include hypens as keyword
set iskeyword-=-

" For the terminal
set t_Co=256
" Solazired doens't work otherwise
set guifont=Monaco:h16
" set background=light
colorscheme monokai 
" colorscheme hemisu 

" Edit and create files
set modifiable
set write

"Rainbow lol
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

" handles multiple buffers
set hidden

" change leader from default \ to ,
let mapleader = ","

" make tab completion of files/buffers act like bash
set wildmenu

" Text and files
set tabstop=2
set shiftwidth=2
set encoding=UTF8
set smartindent
set autoindent
set expandtab

set splitbelow 
set splitright

set showmatch "show matching parenthesis
set ignorecase smartcase "ignore case when searching
set smarttab "insert tabs on the start of a line according to shiftwidth, not tabstop
set incsearch " Highlight matches as you type.
set hlsearch "Highlight matches.

set winwidth=79

" GRB: clear the search buffer when hitting return
:nnoremap <CR> :nohlsearch<cr>

:imap jj <Esc>

"Set wrap, wrap complete words
set wrap
set linebreak

" In visual mode: D will dupliate selected text below
vmap D y'>p

" For autocompletion
set wildmode=list:longest "In directory tree

" Enables folding of test and such
set foldenable

" highlight current line
set cursorline

set cmdheight=2

" Don't show scroll bars in the GUI
set guioptions-=L
set guioptions-=r

" Change the shortcut for zen coding -> ctrl + e
let g:user_zen_expandabbr_key = '<C-e>'

"Map spacebar to colon
nmap <space> :

" Turn of the arrow keys
inoremap  <Up>     <NOP>
inoremap  <Down>   <NOP>
inoremap  <Left>   <NOP>
inoremap  <Right>  <NOP>
noremap   <Up>     <NOP>
noremap   <Down>   <NOP>
noremap   <Left>   <NOP>
noremap   <Right>  <NOP>

" Easier window management
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Map left button to shift left and right button to shift text right
nmap <silent> <Left> <<
nmap <silent> <Right> >>

vmap <silent> <Left> <<
vmap <silent> <Right> >>

" Set leader ft to fold
nnoremap <leader>ft Vatzf

set history=1000 "remember more commands and history
set undolevels=1000 "1000 levels of undo

" Turn off sound
set noerrorbells 
set vb t_vb=
set visualbell

"Remove toolbar
if has("gui_running")
	set guioptions-=T
endif

"Automatically change current directory to that of the file in the buffer
"autocmd BufEnter * cd %:p:h

"c Map for faster autocomplete
imap <leader>ac <C-x><C-o>

"Map for use with command t.
map <leader>f :CommandTFlush<cr>\|:CommandT<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MULTIPURPOSE TAB KEY
" " Indent if we're at the beginning of a line. Else, do completion.
 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! InsertTabWrapper()
 let col = col('.') - 1
 if !col || getline('.')[col - 1] !~ '\k'
   return "\<tab>"
 else
   return "\<c-p>"
 endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-n>
"set dictionary="/usr/dict/words"


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" PROMOTE VARIABLE TO RSPEC LET
" """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! PromoteToLet()
  :normal! dd
  " :exec '?^\s*it\>'
  :normal! P
  :.s/\(\w\+\) = \(.*\)$/let(:\1) { \2 }/
  :normal ==
endfunction
command! PromoteToLet :call PromoteToLet()
map <leader>p :PromoteToLet<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Lets you toggle betweeen relative and absolute numbers
" """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! NumberToggle()
  if(&relativenumber == 1)
    set number
  else
    set relativenumber
  endif
endfunc

nnoremap <C-n> :call NumberToggle()<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" RENAME CURRENT FILE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>n :call RenameFile()<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RENAME CURRENT FILE (thanks Gary Bernhardt)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <Leader>n :call RenameFile()<cr>

map <Leader>nn :sp ~/Dropbox/notes/programming_notes.txt<cr>
map <Leader>rd :!bundle exec rspec % --format documentation<CR>
map <Leader>l oconsole.log 'debugging'<esc>:w<cr>
vmap <Leader>bed "td?describe<cr>obed<tab><esc>"tpkdd/end<cr>o<esc>:nohl<cr>

set backupdir=~/.tmp
set directory=~/.tmp " Don't clutter my dirs up with swp and tmp files
set laststatus=2 " Always show status line.
