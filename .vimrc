" initialize plugins
call plug#begin()
" syntx highighting 
Plug 'slim-template/vim-slim'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'LnL7/vim-nix'
" make searching more convenient
Plug 'romainl/vim-cool'
" needed for yarn berry with immutable flag
Plug 'lbrayner/vim-rzip'
" language server protocol
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" testing at point etc.
Plug 'vim-test/vim-test'
Plug 'airblade/vim-gitgutter'
Plug 'zivyangll/git-blame.vim'
Plug 'google/vim-jsonnet'
Plug 'sebdah/vim-delve'
call plug#end()

set background=dark
set ai                  " auto indenting
set nowrap 
set complete=.,b,u,]
let mapleader = ","
set history=100         " keep 100 lines of history
set ruler               " show the cursor position
set hlsearch            " highlight the last searched term

noremap  <buffer> <silent> k gk
noremap  <buffer> <silent> j gj
nnoremap <leader>! :History:<cr>
nnoremap <leader>f :grep!<Space>
nmap <silent> <leader>t :TestNearest<CR>
nmap <silent> <leader>T :TestFile<CR>
nmap <silent> <leader>a :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>
nmap <silent> <leader>q :copen<CR>
nmap <silent> <leader>Q :ccl<CR>
nmap <silent> <leader>d :Vex<CR>
nmap <silent> <leader>s :<C-u>call gitblame#echo()<CR>

let test#strategy = "vimterminal"
autocmd QuickFixCmdPost *grep* cwindow

" Due to internal representation, Vim has problems with long lines in general.
" https://github.com/mhinz/vim-galore#editing-small-files-is-slow
set synmaxcol=200

if (&tildeop)
  nmap gcc guu~l
else
  nmap gcc guu~h
endif

"tab complete map
inoremap <Tab> <c-r>=InsertTabWrapper()<cr>

"map esc to j k
:imap jk <Esc>

" remap capital Q to rerun last macro
:nmap Q @@

" imap <Tab>, <C-y>,

autocmd Filetype gitcommit setlocal spell textwidth=72
autocmd FileType markdown setlocal spell
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
"set relativenumber
set number
set shiftwidth=2
set expandtab

filetype plugin indent on  

syntax enable

set splitbelow
set splitright

set termguicolors
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

set guifont=Hack:h12
"project specific vim settings
set exrc

command! -nargs=* Wrap set wrap linebreak nolist

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
" If a file is changed outside of vim, automatically reload it without asking
set autoread

autocmd! FileType slim set sw=2 sts=2 et
" Compute syntax highlighting from beginning of file. (By default, vim only
" looks 200 lines back, which can make it highlight code incorrectly in some 
" long files.)
autocmd BufEnter * :syntax sync fromstart
nnoremap <silent><cr> :<c-u>update<cr>
augroup QuickFix
     au FileType qf noremap <buffer> <cr> <cr>
     au FileType qf noremap <buffer> <c-v> <C-w><Enter><C-w>L
augroup END
" automatically make quickfix window span the entire bottom split
autocmd FileType qf wincmd J
"set clipboard=unnamed
set clipboard=unnamedplus
let g:coc_global_extensions = [
  \ 'coc-tsserver',
  \ 'coc-go',
  \ 'coc-json',
  \ 'coc-css'
  \ ]
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gr <Plug>(coc-references)
nnoremap <silent> K :call CocAction('doHover')<CR>

"colors for completion
hi Pmenu ctermbg=0 guibg=black
hi PmenuSel ctermbg=0
hi CocFloating ctermbg=0
"clear out the signcolumn and gitgutter bgs
hi clear SignColumn 
hi clear GitGutterAdd 
hi clear GitGutterChange
hi clear GitGutterDelete
hi clear LineNr
hi LineNr ctermfg=248

"list buffers via fzf
nnoremap <silent><c-b> :<c-u>Buffers<cr>
"set C-P to open fzf
nnoremap <silent><c-p> :<c-u>FZF<cr> 

nnoremap <silent><c-x> :<c-u>q!<cr> 

nnoremap <leader>p :let @+ = expand("%")<cr>
let g:test#javascript#jest#executable = 'yarn jest'
let g:test#javascript#runner = 'jest'
let g:gitgutter_show_msg_on_hunk_jumping = 0

if executable("rg")
    set grepprg=rg\ --vimgrep\ --no-heading\ --hidden
    set grepformat=%f:%l:%c:%m,%f:%l:%m
endif
let g:coc_disable_transparent_cursor = 1

nnoremap <leader>p :set paste<CR>"+p:set nopaste<CR>
