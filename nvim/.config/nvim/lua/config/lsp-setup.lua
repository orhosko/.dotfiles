require'lspconfig'.clangd.setup{
  capabilities = { offsetEncoding = { 'utf-16' } },
}

require'lspconfig'.verible.setup{}

require'lspconfig'.lua_ls.setup{}
