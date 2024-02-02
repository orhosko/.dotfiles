return {
  'nvimtools/none-ls.nvim',
  config = function()
    local none_ls = require('null-ls')
    none_ls.setup({
      sources = {
        -- i'm not sure how to add black to mason install list
        none_ls.builtins.formatting.black,
        none_ls.builtins.formatting.djlint,
      }
    })
  end
}
