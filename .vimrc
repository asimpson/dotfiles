set ai                  " auto indenting
set nowrap 
set complete=.,b,u,]
let mapleader = ","
set history=100         " keep 100 lines of history
set ruler               " show the cursor position
set hlsearch            " highlight the last searched term

"resize pane
nnoremap <leader>d :vertical resize -10<cr>
nnoremap <leader>u :vertical resize +10<cr>
nnoremap <leader>U :res +5<cr>
nnoremap <leader>D :res -5<cr>
nnoremap <leader>r :so $MYVIMRC<cr>
noremap  <buffer> <silent> k gk
noremap  <buffer> <silent> j gj
nnoremap <leader>f :Ag! -Q 
nnoremap <leader>c :noh<cr> 
nnoremap <leader>T :NERDTreeToggle<cr> 
nnoremap <leader>ss :set wrap<cr>
nnoremap <leader>sm :set wm=2<cr>
nnoremap <leader>h :set wm=2<cr>
"http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
nnoremap <CR> G

if (&tildeop)
  nmap gcc guu~l
else
  nmap gcc guu~h
endif

" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<cr>:cw<cr>:wincmd J<cr>

"tab complete map
inoremap <Tab> <c-r>=InsertTabWrapper()<cr>

"map esc to j k
:imap jk <Esc>

" remap capital Q to rerun last macro
:nmap Q @@

"Keymap for zencoding
imap hh <c-y>,

" imap <Tab>, <C-y>,

"Keymap for zencoding
" imap <Tab>p <C-p>

"Leader keymapping for :Tabmerge
nnoremap <leader>m :Tabmerge left<cr>

"Leader keymapping for tab management
nnoremap <leader>n :tabnew<cr>

"Leader keymapping for next tab
nnoremap <leader>t :tabn<cr>
nnoremap <leader>y :tabp<cr>

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

filetype off      " use the file type plugins

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'othree/yajs.vim'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'yssl/QFEnter'
Plugin 'chriskempson/base16-vim'
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'L9'
Plugin 'kchmck/vim-coffee-script'
Plugin 'kien/ctrlp.vim'
Plugin 'tmhedberg/matchit'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'git://github.com/vim-scripts/Tabmerge.git'
Plugin 'airblade/vim-gitgutter'
Plugin 'gregsexton/MatchTag'
Plugin 'rking/ag.vim'
Plugin 'mattn/webapi-vim'
Plugin 'mattn/gist-vim'
Plugin 'vim-scripts/CursorLineCurrentWindow'
Plugin 'vim-scripts/tComment'
Plugin 'noahfrederick/Hemisu'
Plugin 'elzr/vim-json'
Plugin 'mattn/emmet-vim'
Plugin 'henrik/vim-qargs'
Plugin 'mikewest/vimroom'
Plugin 'itspriddle/vim-marked'

call vundle#end()            " required
filetype plugin indent on  

set cursorline
let g:gist_post_private = 1
let g:gist_clip_command = 'pbcopy'

let g:ctrlp_show_hidden = 1

set clipboard+=unnamed " Yanks go on clipboard instead.
syntax on
set background=dark
colorscheme base16-ocean

if executable('ag')
	set grepprg=ag\ --nogroup\ --nocolor
	let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_use_caching = 0
endif

set splitbelow
set splitright

let g:marked_app = "Marked 2"
au BufRead,BufNewFile *.txt set ft=markdown syntax=markdown
au BufRead,BufNewFile *.md set ft=markdown syntax=markdown

"CtrlP setup
let g:ctrlp_cmd = 'CtrlP'
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
" position, order, min height, max height, num of results
let g:ctrlp_match_window = 'bottom,order:btt,min:20,max:20,results:20'

"highlight StatusLine ctermfg=black ctermbg=White cterm=bold
highlight StatusLine cterm=reverse ctermfg=yellow  ctermbg=white
" file path [line number] [encoding] [filetype]
"[%{strlen(&fenc)?&fenc:&enc}]\
set statusline=%F%m%r%h%w\ [%n]\%=\ [line\ %l\/%L]\ [%{&filetype}]

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

set undodir=~/.vim/undodir
set undofile
set undolevels=1000 "maximum number of changes that can be undone
set undoreload=10000 "maximum number lines to save for undo on a buffer reload

set guifont=Source\ Code\ Pro:h11

command! -nargs=* Wrap set wrap linebreak nolist
autocmd QuickFixCmdPost *grep* cwindow
let NERDTreeHijackNetrw=1

"http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
vnoremap <silent> s //e<C-r>=&selection=='exclusive'?'+1':''<CR><CR>
    \:<C-u>call histdel('search',-1)<Bar>let @/=histget('search',-1)<CR>gv
omap s :normal vs<CR>
