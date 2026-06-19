vim.lsp.config('clangd', {
  -- capabilities = { offsetEncoding = { 'utf-16' } },
  -- cmd = { 'clangd', '--experimental-modules-support' },
})
vim.lsp.enable('clangd')

vim.lsp.config('pylsp', {
  on_init = function(client)
    client.server_capabilities.documentFormattingProvider = false -- Disable formatting (formatting is done by stylua)

    if client.workspace_folders then
      local path = client.workspace_folders[1].name
      if path ~= vim.fn.stdpath 'config' and (vim.uv.fs_stat(path .. '/.luarc.json') or vim.uv.fs_stat(path .. '/.luarc.jsonc')) then return end
    end

    client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua, {
      runtime = {
        version = 'LuaJIT',
        path = { 'lua/?.lua', 'lua/?/init.lua' },
      },
      workspace = {
        checkThirdParty = false,
        -- NOTE: this is a lot slower and will cause issues when working on your own configuration.
        --  See https://github.com/neovim/nvim-lspconfig/issues/3189
        library = vim.tbl_extend('force', vim.api.nvim_get_runtime_file('', true), {
          '${3rd}/luv/library',
          '${3rd}/busted/library',
        }),
      },
    })
  end,
})
vim.lsp.enable('pylsp')

vim.lsp.config('lua_ls', {})
vim.lsp.enable('lua_ls')

vim.lsp.config('ts_ls', {})
vim.lsp.enable('ts_ls')

vim.lsp.config('ocamllsp', {})
vim.lsp.enable('ocamllsp')

vim.filetype.add({
  extension = {
    mlir = 'mlir',
  },
})

vim.lsp.config('mlir_lsp_server', {})
vim.lsp.enable('mlir_lsp_server')

vim.lsp.config('verible', {
  cmd = { 'verible-verilog-ls', '--rules_config_search' },
})
vim.lsp.enable('verible')
