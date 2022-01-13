(deftheme monokai
  "Created 2021-03-10.")

(custom-theme-set-faces
 'monokai
 '(cursor ((t (:background "white"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "#fd971f"))))
 '(highlight ((t (:background "firebrick4" :foreground "white"))))
 '(region ((t (:extend t :background "#4e4e4e"))))
 '(shadow ((t (:foreground "#555556"))))
 '(secondary-selection ((t (:extend t :background "#525254"))))
 '(trailing-whitespace ((t (:background "#e74c3c"))))
 '(font-lock-builtin-face ((t (:foreground "#fd971f"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#555556" :slant italic))))
 '(font-lock-constant-face ((t (:inherit font-lock-variable-name-face))))
 '(font-lock-doc-face ((t (:foreground "#7f7f80" :inherit (font-lock-comment-face)))))
 '(font-lock-function-name-face ((t (:foreground "#b6e63e"))))
 '(font-lock-keyword-face ((t (:foreground "#fb2874" :family "Source Code Variable"))))
 '(font-lock-negation-char-face ((t (:foreground "#9c91e4" :inherit (bold)))))
 '(font-lock-preprocessor-face ((t (:foreground "#9c91e4" :inherit (bold)))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#9c91e4" :inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#9c91e4" :inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "yellow green"))))
 '(font-lock-type-face ((t (:foreground "#66d9ef"))))
 '(font-lock-variable-name-face ((t (:foreground "#fd971f"))))
 '(font-lock-warning-face ((t (:inherit (warning)))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:weight bold :underline (:color foreground-color :style line) :foreground "#fd971f"))))
 '(link-visited ((t (:foreground "violet" :inherit (link)))))
 '(fringe ((t (:foreground "#4e4e4e" :inherit (default)))))
 '(header-line ((t (:foreground "#d6d6d4" :background "#1c1e1f"))))
 '(tooltip ((t (:foreground "#d6d6d4" :background "#2d2e2e"))))
 '(mode-line ((t (:box nil :background "#2d2e2e"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:foreground "#fd971f"))))
 '(mode-line-highlight ((t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:box nil :foreground "#4e4e4e" :background "#171819"))))
 '(isearch ((t (:foreground "#1B2229" :background "#b6e63e"))))
 '(isearch-fail ((t (:weight bold :foreground "#1B2229" :background "#e74c3c"))))
 '(lazy-highlight ((t (:weight bold :foreground "#1B2229" :background "#9c91e4"))))
 '(match ((t (:weight bold :foreground "#b6e63e" :background "#1B2229"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(outline-1 ((t (:foreground "#fb2874"))))
 '(org-block ((t (:background "gray8"))))
 '(ido-subdir ((t (:foreground "deepskyblue"))))
 '(eshell-prompt ((t (:foreground "deepskyblue" :weight bold))))
 '(company-tooltip-common ((((background light)) (:foreground "darkred")) (((background dark)) (:foreground "red"))))
 '(company-tooltip ((t (:background "gray10" :foreground "white"))))
 '(company-tooltip-selection ((t (:background "firebrick4"))))
 '(org-quote ((t (:inherit org-block :slant italic :family "Liberation Mono"))))
 '(org-verse ((t (:inherit org-block :slant oblique :family "Liberation Mono"))))
 '(hl-line ((t (:extend t :background "gray9"))))
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "#d6d6d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :foundry "ADBO" :family "ibm plex mono")))))

(provide-theme 'monokai)