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


" start vim-plug
    let plug_installed=1
    let plug_path=expand('~/.vim/autoload/plug.vim')
    if !filereadable(plug_path)
        echo "Installing vim-plug.."
        echo ""
        silent !mkdir -p ~/.vim/autoload
        silent !wget --no-check-certificate -cO ~/.vim/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        let plug_installed=0
    endif

    if plug_installed == 0
        echo "Installing Plugs..."
        echo ""
        :PlugInstall
    endif

    call plug#begin('~/.vim/plugged')

    " Make sure you use single quotes
    Plug 'Raimondi/delimitMate'
    Plug 'SirVer/ultisnips'
    Plug 'scrooloose/syntastic'
    Plug 'majutsushi/tagbar'
    Plug 'fatih/vim-go'
    call plug#end()

    if plug_installed == 0
        echo "Installing Plugs: PlugInstall..."
        echo ""
        :PlugInstall
    endif
" end vim-plug

" start UltiSnips
    let g:UltiSnipsExpandTrigger="<tab>"
    let g:UltiSnipsJumpForwardTrigger="<tab>"
    let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
    let g:ultisnips_python_style="google"
" end UltiSnips
" start syntastic
    let g:syntastic_python_pylint_post_args='--disable=C0103,F0401'
" end syntastic
