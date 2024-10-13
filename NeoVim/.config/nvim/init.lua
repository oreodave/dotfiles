-- Bootstrap lazy.nvim
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
vim.schedule(function()
  vim.opt.clipboard = 'unnamedplus'
end)
vim.opt.termguicolors = true
vim.opt.tabstop       = 2
vim.opt.softtabstop   = 2
vim.opt.shiftwidth    = 2
vim.opt.wildmenu      = true
vim.opt.expandtab     = true
vim.opt.inccommand    = 'split'
vim.opt.cursorline    = true
vim.opt.scrolloff     = 8
vim.opt.autoindent    = true
vim.cmd("set path+=**")

-- Setup lazy.nvim
require("config.lazy")
