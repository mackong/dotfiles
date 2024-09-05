vim.g.mapleader = ','
vim.g.maplocalleader = ','

local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system {
        'git',
        'clone',
        '--filter=blob:none',
        'https://github.com/folke/lazy.nvim.git',
        '--branch=stable', -- latest stable release
        lazypath,
    }
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({
    {
        'ishan9299/modus-theme-vim',
        priority = 1000,
        config = function()
            vim.g.modus_dim_inactive_window = 0
            vim.cmd.colorscheme 'modus-operandi'
        end,
    },

    {
        'nvim-lualine/lualine.nvim',
        opts = {
            options = {
                icons_enabled = false,
                theme = 'auto',
                component_separators = '|',
                section_separators = '',
            },
        },
    },

    {
        'tpope/vim-surround'
    },

    {
        'lfv89/vim-interestingwords'
    },

    {
        'alexghergh/nvim-tmux-navigation'
    },

    {
        'nvim-telescope/telescope.nvim',
        branch = '0.1.x',
        dependencies = {
            'nvim-lua/plenary.nvim',
            'BurntSushi/ripgrep'
        },
    },

    {
        'nvim-telescope/telescope-file-browser.nvim'
    },

    {
         'nvim-treesitter/nvim-treesitter',
         build = ':TSUpdate',
    },
}, {})

-- no backup file
vim.o.backup = false
vim.o.writebackup = false

-- no swap file for buffer
vim.o.swapfile = false

-- maximum width of text that is being inserted
vim.o.textwidth = 120

-- show line number always
vim.o.number = true

-- ignore case when input search pattern
vim.o.ignorecase = true
vim.o.smartcase = true

-- briefly jump to the matching bracket
vim.o.showmatch = true

-- Number of screen lines for command-line
vim.o.cmdheight = 1

-- number of spaces for each step of (auto)indent
vim.o.shiftwidth = 4

-- number of spaces that a <Tab> in the file counts for
vim.o.tabstop = 4

-- number of spaces that a <Tab> counts for while performing editing operations
vim.o.softtabstop = 4

-- use appropriate number of spaces to insert a <Tab>
vim.o.expandtab = true

-- smart indent
vim.o.smartindent = true

-- c indent
vim.o.cindent = true

-- markers are used to specify folds
vim.o.foldmethod = 'marker'

-- files to ignore
vim.o.wildignore = '*/.git/*,*/tmp/*,*.so,*.swp,*.zip,*.pyc'

-- highlight search
vim.o.hlsearch = true

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

vim.o.termguicolors = true
vim.o.background = 'light'

vim.keymap.set('n', '<leader>ee', '<cmd>vsplit ~/.config/nvim/init.lua<cr>', { silent = true })

require('telescope').setup {
    defaults = {
        mappings = {
            i = {
                ['<C-u>'] = false,
                ['<C-d>'] = false,
            },
        },
    },
}
require("telescope").load_extension 'file_browser'

vim.defer_fn(function()
  require('nvim-treesitter.configs').setup {
    ensure_installed = 'all',
    highlight = {
      enable = true,
      disable = function(lang, bufnr)
        return vim.api.nvim_buf_line_count(bufnr) > 5000
      end,
    },
  }
end, 0)

vim.keymap.set('n', '<leader>ff', require('telescope.builtin').find_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>fs', require('telescope.builtin').grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>fg', require('telescope.builtin').live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>fb', require('telescope').extensions.file_browser.file_browser, { desc = '[F]file [B]rowser' })

require('nvim-tmux-navigation').setup {
    keybindings = {
        left = "<C-q>h",
        down = "<C-q>j",
        up = "<C-q>k",
        right = "<C-q>l",
    }
}

