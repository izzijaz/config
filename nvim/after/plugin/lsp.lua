local lsp = require('lsp-zero').preset({})

lsp.on_attach(function(client, bufnr)
    lsp.default_keymaps({ buffer = bufnr })
end)


-- (Optional) Configure lua language server for neovim
local lsp_config = require('lspconfig')
local lsp_defaults = lsp_config.util.default_config

lsp_defaults.capabilities = vim.tbl_deep_extend(
    'force',
    lsp_defaults.capabilities,
    require('cmp_nvim_lsp').default_capabilities()
)

vim.api.nvim_create_autocmd('LspAttach', {
    desc = 'LSP actions',
    callback = function(event)
        local opts = { buffer = event.buf }

        vim.keymap.set('n', 'K', '<cmd>lua require("pretty_hover").hover()<cr>', opts)
        vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', opts)
        vim.keymap.set('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', opts)
        vim.keymap.set('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>', opts)
        vim.keymap.set('n', 'go', '<cmd>lua vim.lsp.buf.type_definition()<cr>', opts)
        vim.keymap.set('n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>', opts)
        vim.keymap.set('n', 'gs', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts)
        vim.keymap.set('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)
        vim.keymap.set({ 'n', 'x' }, '<F3>', '<cmd>lua vim.lsp.buf.format({async = true})<cr>', opts)
        vim.keymap.set('n', '<F4>', '<cmd>lua vim.lsp.buf.code_action()<cr>', opts)

        vim.keymap.set('n', 'do', '<cmd>lua vim.diagnostic.open_float()<cr>', opts)
        vim.keymap.set('n', 'd[', '<cmd>lua vim.diagnostic.goto_prev()<cr>', opts)
        vim.keymap.set('n', 'd]', '<cmd>lua vim.diagnostic.goto_next()<cr>', opts)
    end
})

require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())
require('lspconfig').clangd.setup({
    cmd = {
        "clangd",
        "--background-index",
       -- "--query-driver",
        --"/usr/bin/g++",  -- For C++
        -- "/usr/bin/gcc", --For C.
        -- "/home/izz/.espressif/tools/xtensa-esp32-elf/esp-12.2.0_20230208/**/bin/xtensa-esp32-elf-*", --esp-idf
        "--offset-encoding=utf-16",
    },
    root_dir = lsp_config.util.root_pattern('compile_commands.json', '.git'),
    on_new_config = function(new_config, new_cwd)
        local status, cmake = pcall(require, "cmake-tools")
        if status then
            cmake.clangd_on_new_config(new_config)
        end
    end,
})
lsp.setup()

---
-- Setup haskell LSP
---
lsp.skip_server_setup({ 'hls' })
local haskell_tools = require('haskell-tools')
local hls_lsp = require('lsp-zero').build_options('hls', {})

local hls_config = {
    hls = {
        capabilities = hls_lsp.capabilities,
        on_attach = function(client, bufnr)
            local opts = { buffer = bufnr }

            -- haskell-language-server relies heavily on codeLenses,
            -- so auto-refresh (see advanced configuration) is enabled by default
            vim.keymap.set('n', '<leader>ca', vim.lsp.codelens.run, opts)
            vim.keymap.set('n', '<leader>hs', haskell_tools.hoogle.hoogle_signature, opts)
            vim.keymap.set('n', '<leader>ea', haskell_tools.lsp.buf_eval_all, opts)
        end
    }
}

-- Autocmd that will actually be in charging of starting hls
local hls_augroup = vim.api.nvim_create_augroup('haskell-lsp', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
    group = hls_augroup,
    pattern = { 'haskell' },
    callback = function()
        haskell_tools.start_or_attach(hls_config)

        ---
        -- Suggested keymaps that do not depend on haskell-language-server:
        ---

        -- Toggle a GHCi repl for the current package
        vim.keymap.set('n', '<leader>rr', haskell_tools.repl.toggle, opts)

        -- Toggle a GHCi repl for the current buffer
        vim.keymap.set('n', '<leader>rf', function()
            haskell_tools.repl.toggle(vim.api.nvim_buf_get_name(0))
        end, def_opts)

        vim.keymap.set('n', '<leader>rq', haskell_tools.repl.quit, opts)
    end
})

require("mason").setup({
    ui = {
        border = "none",
    }
})

local has_words_before = function()
    unpack = unpack or table.unpack
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local luasnip = require("luasnip")
luasnip.setup({
    history = false,
    region_check_events = { "CursorHold" }
})

local cmp = require('cmp')
local cmp_kinds = {
    Text = '  ',
    Method = '  ',
    Function = '  ',
    Constructor = '  ',
    Field = '  ',
    Variable = '  ',
    Class = '  ',
    Interface = '  ',
    Module = '  ',
    Property = '  ',
    Unit = '  ',
    Value = '  ',
    Enum = '  ',
    Keyword = '  ',
    Snippet = '  ',
    Color = '  ',
    File = '  ',
    Reference = '  ',
    Folder = '  ',
    EnumMember = '  ',
    Constant = '  ',
    Struct = '  ',
    Event = '  ',
    Operator = '  ',
    TypeParameter = '  ',
}
require('nvim-highlight-colors').setup {
    render = 'background',
    enable_tailwind = true
}
require('luasnip.loaders.from_vscode').lazy_load()
local lspkind = require("lspkind")
cmp.setup({
    snippet = {
        -- REQUIRED - you must specify a snippet engine
        expand = function(args)
            require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        end,
    },
    enabled = function()
        -- disable completion in comments
        local context = require 'cmp.config.context'
        -- keep command mode completion enabled when cursor is in a comment
        if vim.api.nvim_get_mode().mode == 'c' then
            return true
        else
            return not context.in_treesitter_capture("comment")
                and not context.in_syntax_group("Comment")
        end
    end,
    window = {

    },
    view = {
        --entries = { name = 'custom', selection_order = 'near_cursor' }
    },
    formatting = {
        -- fields = { "kind", "abbr" },
        format = lspkind.cmp_format({
            mode = 'text_symbol',
            maxwidth = 60,
            ellipsis_char = "...",
            menu = ({
                path = "[Path]",
                luasnip = "[Snip]",
                nvim_lua = "[Lua]",
                nvim_lsp = "[LSP]",
                buffer = "[Buf]"
            })
        })
    },
    sources = {
        { name = 'path' },
        { name = 'luasnip', keyword_length = 2 },
        { name = 'nvim_lua' },
        { name = 'nvim_lsp' },
        { name = 'buffer',  keyword_length = 3 },
    },
    mapping = {
        ["<CR>"] = cmp.mapping({
            i = function(fallback)
                if cmp.visible() and cmp.get_active_entry() then
                    cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
                else
                    fallback()
                end
            end,
            s = cmp.mapping.confirm({ select = true }),
            c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
        }),
        ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
                -- You could replace the expand_or_jumpable() calls with expand_or_locally_jumpable()
                -- they way you will only jump inside the snippet region
            elseif luasnip.expand_or_locally_jumpable() then
                luasnip.expand_or_jump()
            elseif has_words_before() then
                cmp.complete()
            else
                fallback()
            end
        end, { "i", "s" }),

        ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end, { "i", "s" }),
        ['<C-Space>'] = cmp.mapping.complete(),

        -- ... Your other mappings ...
    },
})
-- `:` cmdline setup.
cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
        { name = 'path' }
    }, {
        {
            name = 'cmdline',
            option = {
                ignore_cmds = { 'Man', '!', "q", 'wq' }
            },
            keyword_length = 3
        }
    })
})
