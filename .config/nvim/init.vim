call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
Plug 'neovim/nvim-lspconfig'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'itchyny/lightline.vim'
Plug 'kassio/neoterm'
Plug 'hrsh7th/nvim-compe'
Plug 'lervag/vimtex'
Plug 'preservim/nerdtree'
Plug 'henrik/vim-indexed-search'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'morhetz/gruvbox'
Plug 'sjl/badwolf'
Plug 'NLKNguyen/papercolor-theme'
Plug 'joshdick/onedark.vim'
call plug#end()

" Gruvbox
let g:gruvbox_italic=1

" Onedark
let g:onedark_hide_endofbuffer=1
let g:onedark_terminal_italics=1

" Papercolor
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

" Lightline
let g:lightline = {
      \ 'active': {
      \   'left': [[ 'mode', 'paste'],
      \            [ 'gitbranch', 'readonly', 'filename', 'modified' ]]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'FugitiveHead'
      \ },
      \ }

" Compe
inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')
inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })

" LSP mappings
nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> gi <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> <C-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> <C-n> <cmd>lua vim.lsp.diagnostic.goto_next()<CR>
nnoremap <silent> <C-p> <cmd>lua vim.lsp.diagnostic.goto_prev()<CR>

" Easy align
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Nerdtree
nmap <C-n> :NERDTreeToggle<CR>

" Vimtex
let g:vimtex_view_method = 'zathura'
let g:Tex_IgnoredWarnings =
            \'Underfull'."\n".
            \'Overfull'."\n"
let g:vimtex_quickfix_ignore_filters = ['Underfull', 'Overfull', 'hyperref Warning']

" Fzf
let mapleader = " "
nnoremap <Leader><Space> :Files<CR>
nnoremap <Leader>bb :Buffers<CR>
nnoremap <Leader>bk <C-^>:bw #<CR>
nnoremap <Leader>s :Rg<Space>
nnoremap <Leader>t :Ttoggle resize=10<CR>

" Neovim
syntax enable
filetype plugin indent on
set hidden splitright splitbelow noshowmode
set cursorline
set nowrap
set lazyredraw         " Don't redraw when using macros (better performance)
set inccommand=nosplit " Preview changes of substitute command :s
set so=3               " Margins when scrolling up-down
set mouse=a            " Keep cursor position when scrolling with mouse
set updatetime=300
set signcolumn=yes
set diffopt+=algorithm:patience
set completeopt=menuone,noselect
let g:tex_flavor = 'latex'
let g:tex_conceal = 'abdmg'

" Highlight lua heredocs in .vim files
let g:vimsyn_embed = 'lPr'

" Open terminal at the bottom
let g:neoterm_default_mod = 'botright'
let g:neoterm_autoinsert = 1

" Mappings
" <leader>ss toggles spell checking
map <leader>ss :setlocal spell!<cr>
" Yank to clipboard with CTRL+c
vnoremap <C-c> "+y
" Auto close brackets
inoremap {<CR> {<CR>}<Esc>O

" Manage line numbers
" set number relativenumber
augroup numbertoggle
  autocmd!
  autocmd WinEnter,BufEnter,FocusGained,InsertLeave * set nu rnu
  autocmd WinLeave,BufLeave,FocusLost,InsertEnter * set nornu
augroup END

" Return to last edit position when opening files
augroup returncursor
  autocmd!
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
augroup END

" Filetype specific configuration
augroup filetypeconfigs
  autocmd!
  autocmd Filetype tex setlocal conceallevel=1 nu syntax spell tw=135 cc=136
  autocmd FileType c setlocal shiftwidth=2 tabstop=2
  autocmd FileType cpp setlocal shiftwidth=2 tabstop=2
  autocmd FileType python nnoremap <F5> :w<cr>:! python %<cr>
augroup END

colorscheme dracula
hi Normal guibg=NONE ctermbg=NONE

lua << EOF
local lsp = require('lspconfig')
--lsp.denols.setup{}
--lsp.pyls.setup{}
lsp.pyright.setup{}
lsp.texlab.setup{}
lsp.clangd.setup{}

require('compe').setup({
    enabled = true;
    autocomplete = true;
    debug = false;
    min_length = 1;
    preselect = 'enable';
    throttle_time = 80;
    source_timeout = 200;
    incomplete_delay = 400;
    max_abbr_width = 100;
    max_kind_width = 100;
    max_menu_width = 100;
    documentation = true;

    source = {
        path = true;
        buffer = true;
        calc = true;
        nvim_lsp = true;
        nvim_lua = true;
        vsnip = true;
    };
})

local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
        return true
    else
        return false
    end
end

_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-n>"
  elseif check_back_space() then
    return t "<Tab>"
  else
    return vim.fn['compe#complete']()
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  else
    return t "<S-Tab>"
  end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
EOF
