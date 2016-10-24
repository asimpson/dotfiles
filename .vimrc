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
nnoremap <leader>c :noh<cr> 
nnoremap <leader>ss :set wrap<cr>
nnoremap <leader>sm :set wm=2<cr>
nnoremap <leader>h :set wm=2<cr>
" set buffer to hidden, then create a vertical split with the contents of the
" file at point
nnoremap <leader>f :set hidden \| :vertical wincmd f<cr>
"http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
nnoremap <CR> G
" Due to internal representation, Vim has problems with long lines in general.
" https://github.com/mhinz/vim-galore#editing-small-files-is-slow
set synmaxcol=200

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
set relativenumber
set shiftwidth=2
set expandtab

filetype off      " use the file type plugins

filetype plugin indent on  

set clipboard+=unnamed " Yanks go on clipboard instead.
syntax enable
set background=dark

set splitbelow
set splitright

au BufRead,BufNewFile *.txt set ft=markdown syntax=markdown
au BufRead,BufNewFile *.md set ft=markdown syntax=markdown
au BufRead,BufNewFile *.hbs set syntax=html

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

set mouse=

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

set guifont=Hack:h10
"project specific vim settings
set exrc

command! -nargs=* Wrap set wrap linebreak nolist
autocmd QuickFixCmdPost *grep* cwindow

function! ToggleColumn()
    let test = &g:colorcolumn

    if test == 80
      set colorcolumn&
      echo "turning column off"
    else
      set cc=80
      echo "turning colummn on"
    endif
endfunction

nmap <silent><leader>l  :call ToggleColumn()<CR>

"https://robots.thoughtbot.com/vim-splits-move-faster-and-more-naturally
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
set backspace=2 " make backspace work like most other apps

"vim talk mods
set wildmenu
