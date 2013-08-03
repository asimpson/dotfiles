set ai                  " auto indenting
set nowrap 
set complete=.,b,u,]
let mapleader = ","
set history=100         " keep 100 lines of history
set ruler               " show the cursor position
set hlsearch            " highlight the last searched term

"resize pane
nnoremap <leader>d :vertical resize -10
nnoremap <leader>u :vertical resize +10

noremap  <buffer> <silent> k gk
noremap  <buffer> <silent> j gj

"tab complete map
inoremap <Tab> <c-r>=InsertTabWrapper()<cr>

"map esc to j k
:imap jk <Esc>

"Keymap for zencoding
imap <Tab>, <C-y>,

"Keymap for zencoding
imap <Tab>p <C-p>

"Leader keymapping for :Tabmerge
nnoremap <leader>m :Tabmerge left

"Leader keymapping for tab management
nnoremap <leader>n :tabnew

"Leader keymapping for next tab
nnoremap <leader>t :tabn

autocmd Filetype gitcommit setlocal spell textwidth=72
set nocompatible
set ignorecase
set nobackup
set nowb
set noswapfile
set nowritebackup
set showcmd
set incsearch  
set laststatus=2
set tabstop=2
set number
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
Bundle 'git://github.com/vim-scripts/Tabmerge.git'
Bundle 'airblade/vim-gitgutter'
Bundle 'gregsexton/MatchTag'
Bundle 'rking/ag.vim'
Bundle 'mattn/webapi-vim'
Bundle 'mattn/gist-vim'

let g:gist_post_private = 1
let g:gist_clip_command = 'pbcopy'

set clipboard+=unnamed " Yanks go on clipboard instead.
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

" Tab completion
" will insert tab at beginning of line,
" will use completion if not at beginning
set wildmode=list:longest,list:full
set complete=.,w,t
function! InsertTabWrapper()
   let col = col('.') - 1
   if !col || getline('.')[col - 1] !~ '\k'
     return "\<tab>"
   else
     return "\<c-p>"
   endif
endfunction

set mouse=a    " Mouse in all modes

" Reset the listchars
set listchars=""
" make tabs visible
set listchars=tab:▸▸
" show trailing spaces as dots
set listchars+=trail:.
" The character to show in the last column when wrap is off and the line
" continues beyond the right of the screen
set listchars+=extends:>
" The character to show in the last column when wrap is off and the line
" continues beyond the right of the screen
set listchars+=precedes:<

