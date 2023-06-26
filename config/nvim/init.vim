"{{{ Plugins

"{{{vim-plug
call plug#begin('~/.config/nvim/plugged')
"}}}

"{{{ telescope
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
"}}}

"{{{ NERD tree
Plug 'scrooloose/nerdtree'
"}}}

"{{{ surround
Plug 'tpope/vim-surround'
"}}}

"{{{ vim-indexed-search
Plug 'henrik/vim-indexed-search'
"}}}

"{{{ bpftrace
Plug 'mmarchini/bpftrace.vim'
"}}}

"{{{ modus
Plug 'ishan9299/modus-theme-vim'
let g:modus_dim_inactive_window=0
"}}}

"{{{ vim-tmux-navigator
Plug 'christoomey/vim-tmux-navigator'
let g:tmux_navigator_no_mappings = 1
"}}}

"{{{ vim-interestingwords
Plug 'lfv89/vim-interestingwords'
"}}}

"{{{ tree-sitter
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
"}}}

call plug#end()
"}}}

"{{{ Generic Settings

" Map Leader Setting
let mapleader=","

" no backup file
set nobackup
set nowritebackup

" no swap file for buffer
set noswapfile

" maximum width of text that is being inserted
set textwidth=120

" show line number always
set number

" ignore case when input search pattern
set ignorecase

" smart case
set smartcase

" briefly jump to the matching bracket
set showmatch

" Number of screen lines for command-line
set cmdheight=1

" disable status line
" set laststatus=0

" number of spaces for each step of (auto)indent
set shiftwidth=4

" number of spaces that a <Tab> in the file counts for
set tabstop=4

" number of spaces that a <Tab> counts for while performing editing operations
set softtabstop=4

" use appropriate number of spaces to insert a <Tab>
set expandtab

" warp long lines
set linebreak

" lines longer than the width of the window will wrap
set wrap

" smart indent
set smartindent

" c indent
if has("cindent")
    set cindent
endif

" mmarkers are used to specify folds
set foldmethod=marker

set wildignore+=*/.git/*,*/tmp/*,*.so,*.swp,*.zip,*.pyc

" Only menu for complete
set completeopt=menu

set termguicolors
colorscheme modus-vivendi
set cursorline

au BufReadPost *
\ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit'
\ | exe "normal! g`\""
\ | endif

" tree-sitter
lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = "all",
  highlight = {
    enable = true
  },
}
EOF


"}}}

"{{{ Key Mappings

" .vimrc edit key map settings
noremap <silent> <leader>ss :source ~/.config/nvim/init.vim<cr>
noremap <silent> <leader>ee :vs ~/.config/nvim/init.vim<cr>

" telescope
nnoremap <leader>ff :lua require('telescope.builtin').find_files({previewer = false})<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fs <cmd>Telescope grep_string<cr>

" Tagbar
noremap <silent> <leader>tb :TagbarToggle<cr>

" NERDTree
noremap <silent> <leader>nt :NERDTreeToggle<cr>

" vim-tmux-navigator
nnoremap <silent> <C-q>h :TmuxNavigateLeft<cr>
nnoremap <silent> <C-q>j :TmuxNavigateDown<cr>
nnoremap <silent> <C-q>k :TmuxNavigateUp<cr>
nnoremap <silent> <C-q>l :TmuxNavigateRight<cr>

"}}}
