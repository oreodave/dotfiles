return {
  "nvim-telescope/telescope.nvim",
  config = function ()
    local telescope = require("telescope.builtin")
    vim.keymap.set("n", "<leader>ff", telescope.find_files, {})
    vim.keymap.set("n", "<leader>ss", telescope.current_buffer_fuzzy_find, {})
  end
}
