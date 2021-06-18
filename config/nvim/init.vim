"{{{ Plugins

"{{{vim-plug
call plug#begin('~/.config/nvim/plugged')
"}}}

"{{{ telescope
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
"}}}

"{{{ Tagbar
Plug 'majutsushi/tagbar'
"}}}

"{{{ NERD tree
Plug 'scrooloose/nerdtree'
"}}}

"{{{ commentary
Plug 'tpope/vim-commentary'
"}}}

"{{{ surround
Plug 'tpope/vim-surround'
"}}}

"{{{ vim-indexed-search
Plug 'henrik/vim-indexed-search'
"}}}

"{{{ vim-indent-line
Plug 'Yggdroot/indentLine'
let g:indentLine_char = '¦'
"}}}

"{{{ molokai
Plug 'tomasr/molokai'
"}}}

"{{{ bpftrace
Plug 'mmarchini/bpftrace.vim'
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

colorscheme molokai

au BufReadPost *
\ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit'
\ | exe "normal! g`\""
\ | endif

"}}}

"{{{ Key Mappings

" Tab Label's Key Map Settings
noremap tc :tabnew<cr>
noremap tn :tabnext<cr>
noremap tp :tabprevious<cr>
noremap tl :tabclose<cr>

" .vimrc edit key map settings
noremap <silent> <leader>ss :source ~/.config/nvim/init.vim<cr>
noremap <silent> <leader>ee :vs ~/.config/nvim/init.vim<cr>

" telescope
nnoremap <leader>ff :lua require('telescope.builtin').find_files({previewer = false})<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>

" Tagbar
noremap <silent> <leader>tb :TagbarToggle<cr>

" NERDTree
noremap <silent> <leader>nt :NERDTreeToggle<cr>

"}}}
