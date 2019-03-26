"{{{ Plugins

"{{{vim-plug
call plug#begin('~/.config/nvim/plugged')
"}}}

"{{{
Plug 'tomasr/molokai'
"}}}

"{{{ LeaderF
Plug 'Yggdroot/LeaderF', { 'do': './install.sh' }
let g:Lf_ShortcutF = '<C-p>'
let g:Lf_ShortcutB = '<A-n>'
let g:Lf_WindowHeight = 0.30
let g:Lf_WorkingDirectoryMode = 'Ac'
"}}}

"{{{ag
Plug 'jremmen/vim-ripgrep'
let g:rg_highlight=1
"}}}

"{{{NERD tree
Plug 'scrooloose/nerdtree'
"}}}

"{{{commentary
Plug 'tpope/vim-commentary'
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

" always show page labels
set showtabline=2

" briefly jump to the matching bracket
set showmatch

" Number of screen lines for command-line
set cmdheight=1

" always show status line
set laststatus=2

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

set cursorline      "highlight the screen line of the cursor with CursorLine

set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc

" Only menu for complete
set completeopt=menu

set termguicolors 

colorscheme molokai

"}}}

"{{{ Key Mappings

" Tab Label's Key Map Settings
noremap tc :tabnew<cr>
noremap tn :tabnext<cr>
noremap tp :tabprevious<cr>
noremap tl :tabclose<cr>

" .vimrc edit key map settings
noremap <silent> <leader>ss :source ~/.config/nvim/init.vim<cr>
noremap <silent> <leader>ee :tabnew ~/.config/nvim/init.vim<cr>
autocmd! bufwritepost .vimrc source ~/.config/nvim/init.vim

" Leaderf
noremap <silent> <leader>ff :LeaderfFunction<cr>
noremap <silent> <leader>FF :LeaderfFunctionAll<cr>
noremap <silent> <leader>tt :LeaderfBufTag<cr>
noremap <silent> <leader>TT :LeaderfTag<cr>

" NERDTree
noremap <silent> <F5> :NERDTreeToggle<cr>

"}}}
