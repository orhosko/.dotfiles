return {
  'folke/which-key.nvim',
  event = "VeryLazy",
  opts = {
    icons = {
      mappings = vim.g.have_nerd_font,
    },

    spec = {
      { 'gr',        group = 'LSP Actions', mode = { 'n' } },

      { '<leader>c', group = '[C]ode',      mode = { 'n', 'x' } },
      { '<leader>d', group = '[D]ocument' },
      { '<leader>r', group = '[R]ename' },
      { '<leader>s', group = '[S]earch' },
      { '<leader>w', group = '[W]orkspace' },
      { '<leader>t', group = '[T]oggle' },
      { '<leader>h', group = 'Git [H]unk',  mode = { 'n', 'v' } },

      { '<leader>n', group = '[N]ote',      mode = { 'n', 'v' } },
      { '<leader>p', group = '[P]roject',   mode = { 'n', 'v' } },
    },
  },
}
