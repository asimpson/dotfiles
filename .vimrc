set ai                  " auto indenting
set history=100         " keep 100 lines of history
set ruler               " show the cursor position
set hlsearch            " highlight the last searched term

"map esc to j k
:imap jk <Esc>

"Keymap for zencoding
imap <Tab>, <C-y>,

"Keymap for zencoding
imap <Tab>p <C-p>

autocmd Filetype gitcommit setlocal spell textwidth=72
set nocompatible
set nobackup
set nowritebackup
set showcmd
set incsearch  
set laststatus=2
set tabstop=2
set shiftwidth=2
set expandtab
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'git://git.wincent.com/command-t.git'
Bundle 'git://github.com/altercation/vim-colors-solarized.git'
Bundle 'kchmck/vim-coffee-script'
Bundle 'kien/ctrlp.vim'
Bundle 'tsaleh/vim-matchit'
Bundle 'vim-scripts/ctags.vim'
Bundle 'git://github.com/nono/vim-handlebars.git'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'git://github.com/mattn/zencoding-vim.git'
Bundle 'desert-warm-256'

filetype off      " use the file type plugins
syntax on
set background=dark
"colorscheme solarized
colorscheme desert-warm-256
filetype plugin indent on

if executable('ag')
	set grepprg=ag\ --nogroup\ --nocolor
	let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

set splitbelow
set splitright

au BufRead,BufNewFile *.handlebars,*.hbs set ft=html syntax=handlebars
"CtrlP setup
let g:ctrlp_cmd = 'CtrlP'
set wildignore+=*/tmp/*,*.so,*.swp,*.zip

"highlight StatusLine ctermfg=black ctermbg=White cterm=bold
highlight StatusLine cterm=reverse ctermfg=136  ctermbg=white
