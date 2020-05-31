;;; ui/telephone/config.el -*- lexical-binding: t; -*-

(use-package! telephone-line
  :hook (after-init . telephone-line-mode)
  :init
  ;; Faces
  (defface +oreoline-accent-dark   '((t (:foreground "black" :background "Cadet Blue"      ))) "")
  (defface +oreoline-evil-dark     '((t (:foreground "white"    :background "dark green"           ))) "")
  (defface +oreoline-evil-inactive '((t (:foreground "cornsilk" :background "gray26"          ))) "")

  (defface +oreoline-accent-light  '((t (:foreground "black"    :background "Light Slate Grey"))) "")
  (defface +oreoline-evil-light    '((t (:foreground "black"    :background "Sky Blue"        ))) "")

  ;; Set telephone line faces
  (setq telephone-line-faces
        '((evil          . (+oreoline-evil-dark   . +oreoline-evil-inactive))
          (modal         . telephone-line-modal-face)
          (ryo           . telephone-line-ryo-modal-face)
          (accent        . (+oreoline-accent-dark . telephone-line-accent-inactive))
          (nil mode-line . mode-line-inactive)))
  (when (featurep! +light)
    (setq telephone-line-faces
          '((evil          . (+oreoline-evil-light   . +oreoline-evil-inactive))
            (modal         . telephone-line-modal-face)
            (ryo           . telephone-line-ryo-modal-face)
            (accent        . (+oreoline-accent-light . telephone-line-accent-inactive))
            (nil mode-line . mode-line-inactive))))

  ;; Seperators
  (setq telephone-line-primary-left-separator    'telephone-line-abs-left
        telephone-line-secondary-left-separator  'telephone-line-identity-hollow-left
        telephone-line-primary-right-separator   'telephone-line-abs-right
        telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)

  ;; LSP segment
  (telephone-line-defsegment +oreoline-lsp-segment ()
    (if (bound-and-true-p lsp-mode)
        (propertize "")
      (propertize "")))

  ;; LHS
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment
                     telephone-line-buffer-modified-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-filesize-segment
                     telephone-line-buffer-name-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . ())))

  ;; RHS
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment
                     +oreoline-lsp-segment
                     telephone-line-flycheck-segment))
          (evil   . (telephone-line-airline-position-segment))))
  :config
  (size-indication-mode +1))

(use-package! evil-anzu
  :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight)
