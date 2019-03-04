packadd minpac
call minpac#init()
call minpac#add('k-takata/minpac', {'type': 'opt'})
call minpac#add('prabirshrestha/async.vim')
call minpac#add('prabirshrestha/asyncomplete.vim')
call minpac#add('prabirshrestha/vim-lsp')
call minpac#add('prabirshrestha/asyncomplete-lsp.vim')
call minpac#add('prabirshrestha/asyncomplete-file.vim')
call minpac#add('xuhdev/vim-latex-live-preview')
call minpac#add('scrooloose/nerdtree')
call minpac#add('itchyny/vim-gitbranch')
call minpac#add('itchyny/lightline.vim')
call minpac#add('dracula/vim')
call minpac#add('morhetz/gruvbox')
call minpac#add('xuhdev/vim-latex-live-preview', { 'for': 'tex' })
call minpac#add('henrik/vim-indexed-search')
packloadall

set cul hid hls is ru wmnu termguicolors ls=2 bg=dark
set tw=80
set timeoutlen=1000 ttimeoutlen=5 " no delay when pressing esc in visual mode
filetype plugin indent on
syntax on

let g:livepreview_cursorhold_recompile = 0
let g:lsp_signs_enabled = 1
let g:lsp_diagnostics_echo_cursor = 1

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"
let g:asyncomplete_smart_completion = 1
let g:asyncomplete_auto_popup = 1

if executable('clangd')
  au User lsp_setup call lsp#register_server({
        \ 'name': 'clangd',
        \ 'cmd': {server_info->['clangd']},
        \ 'whitelist': ['c', 'cpp', 'objc', 'objcpp'],
        \ })
endif

if executable('pyls')
  au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
endif

au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#file#get_source_options({
      \ 'name': 'file',
      \ 'whitelist': ['*'],
      \ 'priority': 10,
      \ 'completor': function('asyncomplete#sources#file#completor')
      \ }))

let g:lightline = {
      \ 'colorscheme': 'powerline',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'gitbranch#name'
      \ },
      \ }

map <C-n> :NERDTreeToggle<cr>

" :W sudo saves the file 
command W w !sudo tee % > /dev/null

" Set 5 lines to the cursor - when moving vertically using j/k
set so=5
" When searching try to be smart about cases 
set smartcase
" Don't redraw while executing macros (good performance config)
set lazyredraw 
" For regular expressions turn magic on
set magic

" hilight text wider than 80 columns
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

set expandtab
set smarttab
set shiftwidth=4
set tabstop=4

" Linebreak on 500 characters
set lbr
set tw=500

set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines

" Visual mode pressing * or # searches for the current selection
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

set splitbelow
set splitright
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-h> <C-w>h
nmap <C-l> <C-w>l

" Return to last edit position when opening files (You want this!)
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" Pressing <leader>ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>
" Move to next misspelled word:       ]s
" Move to previous misspelled word:   [s
" Add word to dictionary:             zg
" Temporarily add word to dictionary: zG
" Remove word from dictionary:        zug
" Remove word from temp dictionary:   zuG
" Show possible corrections:          z=

" set colorscheme
try
    let g:gruvbox_italic=1
    colorscheme gruvbox
catch
    colorscheme desert
endtry
hi! Normal ctermbg=NONE guibg=NONE
hi! Terminal ctermbg=NONE ctermfg=NONE guibg=NONE guifg=NONE

" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    endif
    return ''
endfunction
