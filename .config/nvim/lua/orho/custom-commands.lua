local KBOnlyMode = false
vim.api.nvim_create_user_command('KBOnlyMode',
  function(opts)
    if opts.fargs[1] == 'enable' then
      vim.keymap.set("i", "<Esc>", "")
      vim.keymap.set({ "i", "n", "v" }, "<Up>", "")
      vim.keymap.set({ "i", "n", "v" }, "<Down>", "")
      vim.keymap.set({ "i", "n", "v" }, "<Left>", "")
      vim.keymap.set({ "i", "n", "v" }, "<Right>", "")

      vim.opt.mouse = ""

      -- vim.keymap.set("i", "<k0>", "") -- doesn't work, god knows why

      KBOnlyMode = true
      print('KBOnlyMode enabled')
    elseif opts.fargs[1] == 'disable' then
      vim.keymap.del("i", "<Esc>")
      vim.keymap.del({ "i", "n", "v" }, "<Up>")
      vim.keymap.del({ "i", "n", "v" }, "<Down>")
      vim.keymap.del({ "i", "n", "v" }, "<Left>")
      vim.keymap.del({ "i", "n", "v" }, "<Right>")

      vim.opt.mouse = "a"

      KBOnlyMode = false
      print('KBOnlyMode disabled')
    elseif opts.fargs[1] == 'status' then
      print('KBOnlyMode is ' .. (KBOnlyMode and 'enabled' or 'disabled'))
    else
      print('Invalid argument')
    end
  end,
  {
    nargs = 1,
    complete = function()
      return { "enable", "disable", "status" }
    end,
  })
