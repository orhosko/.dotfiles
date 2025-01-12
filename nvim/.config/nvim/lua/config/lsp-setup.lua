require('lspconfig').clangd.setup({
  capabilities = { offsetEncoding = { 'utf-16' } },
})

require('lspconfig').verible.setup({
  cmd = { 'verible-verilog-ls', '--rules_config_search' },
})

require('lspconfig').lua_ls.setup({})
