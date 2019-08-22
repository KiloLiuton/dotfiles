call plug#begin('~/.vim/plugged')
Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}
" Plug 'pangloss/vim-javascript'
" Plug 'neoclide/vim-jsx-improve'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
" Plug 'editorconfig/editorconfig-vim'
Plug 'jpalardy/vim-slime'
Plug 'junegunn/fzf.vim'
Plug 'henrik/vim-indexed-search'
Plug 'morhetz/gruvbox'
Plug 'lervag/vimtex'
"Plug 'donRaphaco/neotex', { 'for': 'tex' }
call plug#end()

" PLUGIN CONFIGS
" let g:javascript_plugin_flow = 1


" vimtex configs
let g:vimtex_view_method = 'zathura'

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" Navigate snippet placeholders using tab
let g:coc_snippet_next = '<Tab>'
let g:coc_snippet_prev = '<S-Tab>'

" Use enter to accept snippet expansion
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<CR>"

" Slime target
let g:slime_target = "kitty"

" CONFIGS
set hidden ruler showcmd laststatus=2 cmdheight=2 splitright splitbelow
set autoindent
set diffopt+=algorithm:patience
" Set tab options
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
" Margins when scrolling up-down
set so=5

" Don't redraw while using macros (for better performance)
set lazyredraw

" Return to last edit position when opening files
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" Pressing <C-l> will fix the previous spelling mistake on current line
inoremap <C-l> <c-g>u<Esc>[s1z=`]a<c-g>u
inoremap <C-l> <c-g>u<Esc>:call <sid>fixLineSpellError()<cr>`]a<c-g>u
function! s:fixLineSpellError()
  " get current line number
  let lnum = line('.')
  " find last misspelled word before cursor
  normal! [s
  " do nothing if line changed
  if lnum != line('.') | return | endif
  " fix spell error if line doesn't change
  normal! 1z=
endfunction

" Pressing <leader>ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>
autocmd FileType tex syntax spell toplevel
autocmd FileType tex setlocal nu spell
" tex configs
let g:tex_flavor = 'latex'
let g:tex_conceal = 'abdmg'
set conceallevel=1

" execute python script with leader-enter
autocmd FileType python nnoremap <leader><cr> :w<cr>:! python %<cr>

" Pressing * or # in visual mode searches for the current selection
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

" Format the statusline
set statusline =
" Buffer number
set statusline +=[%n]
" File description
set statusline +=%f\ %h%m%r%w
" Filetype
set statusline +=%y                                                  
" Name of the current branch (needs fugitive.vim)
set statusline +=\ %{fugitive#statusline()}
" Date of the last time the file was saved
set statusline +=\ %{strftime(\"[%d/%m/%y\ %T]\",getftime(expand(\"%:p\")))} 
" Total number of lines in the file
set statusline +=%=%-10L
" Line, column and percentage
set statusline +=%=%-14.(%l,%c%V%)\ %P

" Set colorscheme
let g:gruvbox_italic=1
colorscheme gruvbox
hi Normal guibg=NONE ctermbg=NONE
