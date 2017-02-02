set nocompatible     "not compatible with vi
set backspace=indent,eol,start
set number
"set paste
set ruler
set autoindent       "please place the autoindent after paste

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab        "replace tab with space
set smarttab
set list
set listchars=tab:>-,trail:-

if exists('+colorcolumn')
  set colorcolumn=80
else
  au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)
endif
set cursorline       "highlight current line
set scrolloff=6

set autochdir
set foldenable
set foldmethod=indent
set fml=7
set foldnestmax=2

set hlsearch
set incsearch

set encoding=utf-8
set fileencoding=utf-8

syntax on
filetype plugin on

highlight SpellBad cterm=italic ctermbg=14
set updatetime=500
autocmd CursorHold * silent! exe printf('match incsearch /\<%s\>/', expand('<cword>'))
autocmd CursorHold * silent! foldopen
"autocmd CursorHold * :let @/='\<<C-R>=expand("<cword>")<CR>\>'<CR>:set hls<CR>
"autocmd CursorHold * :let @/='\<'.expand('<cword>').'\>'
"autocmd CursorHold * exe printf(':let @/=\<"%s"\>', expand('<cword>'))

map <C-F12> :!python %
nmap <F8> :TagbarToggle<CR>

if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\""
endif