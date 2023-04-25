(deftheme personal-primary
  "Created 2021-10-20.")

(custom-theme-set-faces
 'personal-primary
 '(line-number ((t (:foreground "gray45" :background "gray1"))))
 '(line-number-current-line ((t (:foreground "white" :background "gray1"))))
 '(child-frame-border ((t (:background "white"))))
 '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "white"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "medium blue"))))
 '(highlight ((t (:background "gray14"))))
 '(region ((t (:extend t :background "gray25"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:extend t :background "yellow1")) (((class color) (min-colors 88) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:extend t :background "yellow")) (((class color) (min-colors 16) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 8)) (:extend t :foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")))) ;;(t (:inverse-video t))))
 '(font-lock-builtin-face ((t (:foreground "powder blue"))))
 '(font-lock-comment-delimiter-face ((t (:slant italic :foreground "gray24"))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#868686"))))
 '(font-lock-constant-face ((t (:foreground "indian red" :weight semi-bold :height 0.99 :family "Source Code Pro"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:weight semi-bold))))
 '(font-lock-keyword-face ((t (:foreground "spring green" :weight bold :height 0.95 :family "Fira Code"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:family "Source Code Pro" :foreground "#868686"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:family "Fira Code" :foreground "yellow4"))))
 '(font-lock-type-face ((t (:foreground "deepskyblue" :family "Liberation Mono"))))
 '(font-lock-variable-name-face ((nil (:family "Source Code Pro" :foreground "white"))))
 '(font-lock-warning-face ((t (:inherit (error)))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "cyan1"))))
 '(link-visited ((t (:foreground "violet" :inherit (link)))))
 '(fringe ((t (:background "grey4"))))
 '(header-line ((t (:box nil :foreground "grey90" :background "grey20" :inherit (mode-line)))))
 '(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
 '(mode-line ((t (:box nil :foreground "LightSkyBlue" :background "black"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:weight light :foreground "CadetBlue" :background "grey7" :inherit (mode-line)))))
 '(isearch ((t (:foreground "brown4" :background "white"))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(company-tooltip ((t (:background "black" :foreground "white"))))
 '(company-tooltip-selection ((t (:background "gray31" :slant italic))))
 '(company-tooltip-annotation ((t (:foreground "grey" :slant italic))))
 '(company-preview ((((background light)) (:inherit (company-tooltip-selection company-tooltip))) (((background dark)) (:foreground "wheat" :background "blue4"))))
 '(company-preview-common ((t (:inherit company-preview :foreground "grey"))))
 '(org-block ((t (:background "gray3" :inherit shadow))))
 '(eshell-prompt ((t (:foreground "deep sky blue" :weight bold))))
 '(whitespace-tab ((t (:background "gray5" :foreground "gray20"))))
 '(whitespace-space ((t (:background "gray2" :foreground "darkgray"))))
 '(whitespace-line ((t (:background "black" :foreground "violet"))))
 '(haskell-interactive-face-prompt ((t (:foreground "green"))))
 '(org-verbatim ((t (:foreground "red3"))))
 '(org-code ((t (:foreground "green3"))))
 '(ivy-current-match ((t (:weight bold :underline t :slant italic))))
 '(orderless-match-face-0 ((t (:weight bold :foreground "lime green"))))
 '(orderless-match-face-1 ((t (:weight bold :foreground "light green"))))
 '(orderless-match-face-2 ((t (:weight bold :foreground "forest green"))))
 '(orderless-match-face-3 ((t (:weight bold :foreground "dark green"))))
 '(pdf-isearch-batch ((t (:foreground "black" :background "white"))))
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :width normal :weight normal :slant normal :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil :foreground "#b6b6b6" :background "gray5" :stipple nil :inherit nil)))))

(provide-theme 'personal-primary)
