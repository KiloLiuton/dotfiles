call plug#begin('~/.vim/plugged')
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'henrik/vim-indexed-search'
Plug 'lervag/vimtex'
"Plug 'tpope/vim-vinegar'
"Plug 'junegunn/fzf.vim'
"Plug 'sheerun/vim-polyglot'
Plug 'morhetz/gruvbox'
Plug 'joshdick/onedark.vim'
Plug 'sjl/badwolf'
Plug 'NLKNguyen/papercolor-theme'
call plug#end()

" BEGIN PLUGIN CONFIGS

" VIMTEX CONFIGS
let g:vimtex_view_method = 'zathura'
let g:Tex_IgnoredWarnings =
            \'Underfull'."\n".
            \'Overfull'."\n"
let g:vimtex_quickfix_ignore_filters = ['Underfull', 'Overfull', 'hyperref Warning']
" END VIMTEX CONFIGS

" COC CONFIGS
" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Navigate snippet placeholders using tab
let g:coc_snippet_next = '<Tab>'
let g:coc_snippet_prev = '<S-Tab>'

" Use enter to accept snippet expansion
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<CR>"

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
" END COC CONFIGS

" END PLUGIN CONFIGS

" BEGIN NVIM CONFIGS
set hidden ruler showcmd laststatus=2 cmdheight=2 splitright splitbelow
set autoindent cursorline
set diffopt+=algorithm:patience
filetype plugin on
set nowrap
" Set tab options
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set so=3  "Margins when scrolling up-down
set updatetime=300
"set signcolumn=yes
set lazyredraw  "Don't redraw when using macros (for better performance)

" Filetype specific commands
filetype on
autocmd Filetype tex set tw=135 cc=136

" Return to last edit position when opening files
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" Fold toggle with spacebar
nnoremap <space> za
vnoremap <space> zf

" Pressing <leader>ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>
autocmd FileType tex syntax spell toplevel
autocmd FileType tex setlocal nu spell
" tex configs
let g:tex_flavor = 'latex'
let g:tex_conceal = 'abdmg'
set conceallevel=1

" Pressing * or # in visual mode searches for the current selection
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

" execute python script with <F5>, equivalent to  `:! python %`
autocmd FileType python nnoremap <F5> :w<cr>:! python %<cr>

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

" Set colorscheme and appearence
" hi Normal guibg=NONE ctermbg=NONE

" GRUVBOX scheme
let g:gruvbox_italic=1
colorscheme gruvbox

" ONEDARK colorscheme
"let g:onedark_hide_endofbuffer=1
"let g:onedark_terminal_italics=1
"colorscheme onedark

" BADWOLF colorscheme
" colorscheme badwolf

" PAPERCOLOR colorscheme
let g:PaperColor_Theme_Options = {
  \   'language': {
  \     'python': {
  \       'highlight_builtins' : 1
  \     },
  \     'cpp': {
  \       'highlight_standard_library': 1
  \     },
  \     'c': {
  \       'highlight_builtins' : 1
  \     }
  \   }
  \ }
set background=dark
colorscheme PaperColor

" END NVIM CONFIGS
