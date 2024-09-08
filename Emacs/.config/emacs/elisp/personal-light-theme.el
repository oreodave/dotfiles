(deftheme personal-light
  "Created 2024-08-24.")

(custom-theme-set-faces
 'personal-light
 '(button ((t (:inherit (link)))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(eshell-ls-directory ((t (:foreground "DeepSkyBlue3" :weight bold))))
 '(eshell-prompt ((t (:foreground "turquoise3" :weight bold))))
 '(evil-mc-cursor-default-face ((t (:foreground "white" :background "black"))))
 '(evil-mc-region-face ((t (:extend t :background "#94a2b2"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(font-lock-bracket-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-builtin-face ((t (:foreground "#A020F0"))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#204A87"))))
 '(font-lock-constant-face ((t (:foreground "#F5665D"))))
 '(font-lock-delimiter-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
 '(font-lock-escape-face ((t (:inherit (font-lock-regexp-grouping-backslash)))))
 '(font-lock-function-call-face ((t (:inherit (font-lock-function-name-face)))))
 '(font-lock-function-name-face ((t (:weight bold :foreground "#00578E"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "#A52A2A"))))
 '(font-lock-misc-punctuation-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-number-face ((t nil)))
 '(font-lock-operator-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-property-name-face ((t (:inherit (font-lock-variable-name-face)))))
 '(font-lock-property-use-face ((t (:inherit (font-lock-property-name-face)))))
 '(font-lock-punctuation-face ((t nil)))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#4E9A06"))))
 '(font-lock-type-face ((t (:weight bold :foreground "#2F8B58"))))
 '(font-lock-variable-name-face ((t (:weight bold :foreground "#0084C8"))))
 '(font-lock-variable-use-face ((t (:inherit (font-lock-variable-name-face)))))
 '(font-lock-warning-face ((t (:weight bold :foreground "#F5666D"))))
 '(fringe ((t (:background "#E6E6E6"))))
 '(header-line ((t (:background "#CCCCCC" :foreground "black"))))
 '(highlight ((t (:foreground "white" :background "#4A90D9"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(isearch ((t (:foreground "white" :background "#77A4DD"))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:distant-foreground "black" :background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:distant-foreground "white" :background "paleturquoise4")) (((class color) (min-colors 16)) (:distant-foreground "white" :background "turquoise3")) (((class color) (min-colors 8)) (:distant-foreground "white" :background "turquoise3")) (t (:underline (:color foreground-color :style line :position nil)))))
 '(link ((t (:underline (:color foreground-color :style line :position nil) :foreground "#0066CC"))))
 '(link-visited ((t (:underline (:color foreground-color :style line :position nil) :foreground "#6799CC"))))
 '(magit-diff-context-highlight ((t (:inherit default))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "khaki1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(minibuffer-prompt ((t (:weight bold :foreground "#0084C8"))))
 '(mode-line ((t (:box (:line-width (1 . -1) :color nil :style released-button) :foreground "#2E3436" :background "white"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width (2 . 2) :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:background "white" :foreground "#929292"))))
 '(next-error ((t (:inherit (region)))))
 '(org-block ((t (:inherit default))))
 '(org-code ((t (:foreground "green4"))))
 '(org-hide ((t (:foreground "#EDEDED"))))
 '(org-quote ((t (:slant italic))))
 '(org-verbatim ((t (:foreground "red3"))))
 '(query-replace ((t (:inherit (isearch)))))
 '(region ((t (:extend t :background "#C2D5E9"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:extend t :background "yellow1")) (((class color) (min-colors 88) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:extend t :background "yellow")) (((class color) (min-colors 16) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 8)) (:extend t :foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light))
            (:foreground "grey50"))
           (((class color grayscale) (min-colors 88) (background dark))
            (:foreground "grey70"))
           (((class color) (min-colors 8) (background light))
            (:foreground "green"))
           (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(tab-bar ((t (:background "white"))))
 '(tab-bar-tab ((t (:background "white" :foreground "grey5" :box (:line-width 2 :color "grey19")))))
 '(tab-bar-tab-inactive ((t (:background "grey70" :foreground "black"))))
 '(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(whitespace-line ((t (:inherit default :background "#e3e3e3" :foreground "violet"))))
 '(whitespace-space ((t (:inherit default :background "#e3e3e3" :foreground "blue"))))
 '(whitespace-tab ((t (:inherit default :background "#e3e3e3" :foreground "grey20"))))
 '(default ((t (:family "RecMonoDuotone Nerd Font Propo" :foundry "ADBO"
                :width normal :weight normal :slant normal :underline nil
                :overline nil :extend nil :strike-through nil :box nil :inverse-video nil
                :foreground "#2E3436" :background "#EDEDED" :stipple nil :inherit
                nil)))))

(provide-theme 'personal-light)
