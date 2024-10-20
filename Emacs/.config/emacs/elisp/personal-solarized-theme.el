(deftheme personal-solarized
  "Created 2024-07-02.")

(defvar personal-solarized-name-colour "#3c98e0"
  "Colour of names in this theme.")

(custom-theme-set-faces
 'personal-solarized
 '(button ((t (:inherit (link)))))
 '(child-frame-border ((t (:background "white"))))
 '(company-preview ((t (:foreground "wheat" :background "blue4"))))
 '(company-preview-common ((t (:inherit company-preview :foreground "grey"))))
 '(company-tooltip ((t (:background "black" :foreground "white"))))
 '(company-tooltip-annotation ((t (:foreground "grey"))))
 '(company-tooltip-selection ((t (:background "grey31"))))
 '(cursor ((t (:background "#f2eec4"))))
 '(dired-ignored ((t (:background "grey10" :underline t))))
 '(escape-glyph ((t (:foreground "cyan"))))
 '(eshell-ls-directory ((t (:foreground "DeepSkyBlue3" :weight bold))))
 '(eshell-prompt ((t (:foreground "turquoise3" :weight bold))))
 '(evil-goggles-default-face ((t (:background "#004065"))))
 '(evil-mc-cursor-default-face ((t (:foreground "black" :background "white"))))
 '(evil-mc-region-face ((t (:extend t :background "grey50"))))
 '(fill-column-indicator ((t (:inherit shadow :foreground "gray23"
                              :background "gray23" :weight thin))))
 '(fixed-pitch-serif ((t (:family "Noto Serif" :height 0.95))))
 '(font-latex-bold-face ((t (:weight bold :foreground "#9eacac"))))
 '(font-latex-doctex-documentation-face ((t (:background unspecified))))
 '(font-latex-doctex-preprocessor-face ((t (:inherit (font-latex-doctex-documentation-face
                                                      font-lock-builtin-face
                                                      font-lock-preprocessor-face)))))
 '(font-latex-italic-face ((t (:style italic :foreground "#9eacac"))))
 '(font-latex-math-face ((t (:foreground "#7a7ed2"))))
 '(font-latex-sectioning-0-face ((t (:inherit font-latex-sectioning-5-face
                                     :height 1.1))))
 '(font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-5-face
                                     :height 1.1))))
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-5-face
                                     :height 1.1))))
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-5-face
                                     :height 1.1))))
 '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face
                                     :height 1.1))))
 '(font-latex-sectioning-5-face ((t (:foreground "#c49619" :weight bold))))
 '(font-latex-sedate-face ((t (:foreground "#9eacac"))))
 '(font-latex-slide-title-face ((t (:inherit (default font-lock-type-face)
                                    :weight bold :height 1.2))))
 '(font-latex-string-face ((t (:foreground "#3cafa5"))))
 '(font-latex-subscript-face ((t (:height 0.8))))
 '(font-latex-superscript-face ((t (:height 0.8))))
 '(font-latex-verbatim-face ((t (:inherit fixed-pitch-serif :foreground "#8d9fa1"
                                 :slant italic))))
 '(font-latex-warning-face ((t (:inherit bold :foreground "#db5823"))))
 '(font-lock-builtin-face ((t (:foreground "#8d9fa1" :weight bold :slant normal))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#62787f" :slant normal))))
 '(font-lock-comment-face ((t (:foreground "#62787f"))))
 '(font-lock-doc-face ((t (:foreground "#3cafa5" :slant normal))))
 '(font-lock-keyword-face ((t (:foreground "#93a61a" :weight bold))))
 '(font-lock-negation-char-face ((t (:foreground "#c49619" :weight bold))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#93a61a" :weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#c49619" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#3cafa5"))))
 '(font-lock-type-face ((t (:foreground "#c49619"))))
 '(font-lock-warning-face ((t (:inherit error :weight bold))))
 '(fringe ((t (:inherit (default)))))
 '(haskell-interactive-face-prompt ((t (:foreground "green"))))
 '(header-line ((t (:box nil :foreground "grey90" :background "grey20"
                    :inherit (mode-line)))))
 '(highlight ((t (:extend t :background "#222233"))))
 '(homoglyph ((t (:foreground "cyan"))))
 '(Info-quoted ((t (:inherit fixed-pitch-serif :underline t))))
 '(isearch ((t (:foreground "brown4" :background "white"))))
 '(isearch-fail ((t (:background "red4"))))
 '(lazy-highlight ((t (:background "paleturquoise4"))))
 '(line-number ((t (:foreground "grey45" :background "grey1" :inherit (default)))))
 '(line-number-current-line ((t (:foreground "white" :background "grey1"
                                 :inherit (default)))))
 '(link ((t (:underline (:color foreground-color :style line)
             :foreground "cyan1"))))
 '(link-visited ((t (:foreground "violet" :inherit (link)))))
 '(match ((t (:background "RoyalBlue3"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(mode-line ((t (:box (:line-width 1 :color "white") :foreground "LightSkyBlue"
                  :background "black" :inherit (default)))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-inactive ((t (:box (:line-width 1 :color "grey10") :weight light
                           :foreground "CadetBlue" :background "grey7"
                           :inherit (default)))))
 '(next-error ((t (:inherit (region)))))
 '(orderless-match-face-0 ((t (:weight bold :foreground "lime green"))))
 '(orderless-match-face-1 ((t (:weight bold :foreground "light green"))))
 '(orderless-match-face-2 ((t (:weight bold :foreground "forest green"))))
 '(orderless-match-face-3 ((t (:weight bold :foreground "dark green"))))
 '(org-block ((t (:inherit default))))
 '(org-code ((t (:foreground "green3"))))
 '(org-hide ((t (:foreground "black"))))
 '(org-quote ((t (:slant italic))))
 '(org-verbatim ((t (:foreground "red3"))))
 '(outline-1 ((t (:inherit default :foreground "#db5823"))))
 '(outline-2 ((t (:inherit default :foreground "#93a61a"))))
 '(outline-3 ((t (:inherit default :foreground "#3c98e0"))))
 '(outline-4 ((t (:inherit default :foreground "#c49619"))))
 '(outline-5 ((t (:inherit default :foreground "#3cafa5"))))
 '(outline-6 ((t (:inherit default :foreground "#93a61a"))))
 '(outline-7 ((t (:inherit default :foreground "#ec423a"))))
 '(outline-8 ((t (:inherit default :foreground "#3c98e0"))))
 '(outline-minor-0 ((t (:extend t :weight bold :background "#01323d"))))
 '(outline-minor-1 ((t (:extend t :inherit (outline-minor-0 outline-1)
                        :background "#1e9d310d32a3"))))
 '(pdf-isearch-batch ((t (:foreground "black" :background "white"))))
 '(pretty-mode-symbol-face ((t (:foreground "#c49619" :weight normal))))
 '(query-replace ((t (:inherit (isearch)))))
 '(rainbow-delimiters-base-error-face ((t (:extend t :foreground "white"
                                           :background "red1"))))
 '(rainbow-delimiters-depth-1-face ((t (:extend t :foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:extend t :foreground "darkorange"))))
 '(rainbow-delimiters-depth-3-face ((t (:extend t :foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:extend t :foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:extend t :foreground "DeepSkyBlue"))))
 '(rainbow-delimiters-depth-6-face ((t (:extend t :foreground "purple"))))
 '(rainbow-delimiters-depth-7-face ((t (:extend t :foreground "violet"))))
 '(rainbow-delimiters-mismatched-face ((t (:extend t :foreground "white"
                                           :background "red4"))))
 '(rainbow-delimiters-unmatched-face ((t (:extend t :foreground "white"
                                          :background "red3"))))
 '(region ((t (:extend t :background "grey25"))))
 '(secondary-selection ((t (:extend t :background "SkyBlue4"))))
 '(shadow ((t (:foreground "grey70"))))
 '(show-paren-match ((t (:foreground unspecified :foreground "#55ec55"
                         :weight bold))))
 '(show-paren-mismatch ((t (:foreground "#01323d" :background "#ec1111"
                            :weight bold))))
 '(tab-bar ((t (:background "black"))))
 '(tab-bar-tab ((t (:background "grey5" :foreground "white"
                    :box (:line-width 2 :color "grey19")))))
 '(tab-bar-tab-inactive ((t (:background "black" :foreground "DimGrey"))))
 '(tooltip ((t (:foreground "black" :background "lightyellow"
                :inherit (variable-pitch)))))
 '(trailing-whitespace ((t (:background "red1"))))
 '(whitespace-line ((t (:background "black" :foreground "violet"))))
 '(whitespace-space ((t (:background "#171717" :foreground "black"))))
 '(whitespace-tab ((t (:background "#171717" :foreground "grey40"))))
 `(font-lock-constant-face ((t (:foreground ,personal-solarized-name-colour
                                :weight bold))))
 `(font-lock-function-name-face ((t (:box nil
                                     :foreground ,personal-solarized-name-colour))))
 `(font-lock-preprocessor-face ((t (:foreground ,personal-solarized-name-colour))))
 `(font-lock-variable-name-face ((t (:foreground ,personal-solarized-name-colour))))
 '(default ((t (:family "Jetbrains Mono" :foundry "ADBO" :width normal
                :weight normal :slant normal :underline nil
                :overline nil :extend nil :strike-through nil
                :box nil :inverse-video nil
                :foreground "#b6b6b6" :background "#0a0a0a"
                :stipple nil :inherit nil)))))

(provide-theme 'personal-solarized)
