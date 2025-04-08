require('lspconfig').clangd.setup{
  capabilities = { offsetEncoding = { 'utf-16' } },
  cmd = { "clangd", "--experimental-modules-support" },
  -- cmd = { "/home/berkay/Documents/projects/llvm-project/build/bin/clangd","--experimental-modules-support" }
} 

require'lspconfig'.verible.setup{
  cmd = {'verible-verilog-ls', '--rules_config_search'},
}

require('lspconfig').pylsp.setup{}

require('lspconfig').mlir_lsp_server.setup{}
vim.filetype.add({
  extension = {
    mlir = 'mlir',
  }
})

-- require'lspconfig'.lua_ls.setup{}
