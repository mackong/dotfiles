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
        'miikanissi/modus-themes.nvim',
        priority = 1000,
        config = function()
            vim.cmd.colorscheme 'modus_operandi'
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
        'neovim/nvim-lspconfig',
    },

    {
        'hrsh7th/nvim-cmp',
        dependencies = {
            'L3MON4D3/LuaSnip',
            'saadparwaiz1/cmp_luasnip',
            'hrsh7th/cmp-nvim-lsp',
        },
    },

    {
        'tpope/vim-surround'
    },

    {
        'lfv89/vim-interestingwords'
    },

    {
        'will133/vim-dirdiff'
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

vim.cmd('autocmd BufEnter * set formatoptions-=cro')
vim.cmd('autocmd BufEnter * setlocal formatoptions-=cro')

vim.cmd('autocmd FileType cpp set shiftwidth=8')
vim.cmd('autocmd FileType cpp set tabstop=8')
vim.cmd('autocmd FileType cpp set softtabstop=8')

-- [[ Configure lsp ]]
local on_attach = function(_, bufnr)
    local nmap = function(keys, func, desc)
        if desc then
            desc = 'LSP: ' .. desc
        end

        vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
    end

    nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
    nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
    nmap('gD', vim.lsp.buf.type_definition, 'Type [D]efinition')
    nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
    nmap('gI', require('telescope.builtin').lsp_implementations, '[G]oto [I]mplementation')
    nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
end

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

local lsp_setup = function(server_name, server_settings)
    require('lspconfig')[server_name].setup {
        capabilities = capabilities,
        on_attach = on_attach,
        settings = server_settings,
    }
end

local servers = {
    clangd = {},
    gopls = {},
    pylsp = {},
    rust_analyzer = {},
}
for server_name, server_settings in pairs(servers) do
    lsp_setup(server_name, server_settings)
end

-- [[ Configure nvim-cmp ]]
local cmp = require 'cmp'
local luasnip = require 'luasnip'
require('luasnip.loaders.from_vscode').lazy_load()
luasnip.config.setup {}

cmp.setup {
    completion = {
        autocomplete = false,
    },
    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body)
        end,
    },
    mapping = cmp.mapping.preset.insert {
        ['<C-n>'] = cmp.mapping.select_next_item(),
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete {},
        ['<CR>'] = cmp.mapping.confirm {
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
        },
        ['<Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            elseif luasnip.expand_or_locally_jumpable() then
                luasnip.expand_or_jump()
            else
                fallback()
            end
        end, { 'i', 's' }),
        ['<S-Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            elseif luasnip.locally_jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end, { 'i', 's' }),
    },
    sources = {
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
    },
}
