return {
  "overcache/NeoSolarized",
  init = function ()
    vim.cmd.colorscheme "NeoSolarized"
    -- Make background transparent
    vim.cmd.hi "Normal guibg=none"
    vim.cmd.hi "NonText guibg=none"
    vim.cmd.hi "Normal ctermbg=none"
    vim.cmd.hi "NonText ctermbg=none"
  end
}
