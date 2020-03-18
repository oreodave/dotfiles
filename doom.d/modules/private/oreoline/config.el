;;; ui/telephone/config.el -*- lexical-binding: t; -*-

(use-package! telephone-line
  :hook (after-init . telephone-line-mode)
  :init
  ; Colors/Faces
  (defface my-accent-dark    '((t (:foreground "white" :background "black"))) "")
  (defface my-evil-dark      '((t (:foreground "black" :background "dim grey"))) "")
  (defface my-accent-light   '((t (:foreground "black" :background "dim grey"))) "")
  (defface my-evil-light     '((t (:foreground "white" :background "blue"))) "")

  ; Seperators
  (setq telephone-line-primary-left-separator    'telephone-line-halfsin-left)
  (setq telephone-line-faces
        '((evil . (my-evil-dark . my-evil-dark))
          (modal . telephone-line-modal-face)
          (ryo . telephone-line-ryo-modal-face)
          (accent . (my-accent-dark . my-accent-dark))
          (nil mode-line . mode-line-inactive)))
  (when (featurep! +light)
    (setq telephone-line-faces
          '((evil . (my-evil-light . my-evil-light))
            (modal . telephone-line-modal-face)
            (ryo . telephone-line-ryo-modal-face)
            (accent . (my-accent-light . my-accent-light))
            (nil mode-line . mode-line-inactive))))

  ; LSP segment
  (telephone-line-defsegment +oreoline-lsp-segment ()
    (after! lsp-mode
      (if (lsp-workspaces)
          (propertize "")
        (propertize ""))))

  ; LHS
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment
                     telephone-line-buffer-modified-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-filesize-segment
                     telephone-line-buffer-name-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . ())))

  ; RHS
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
