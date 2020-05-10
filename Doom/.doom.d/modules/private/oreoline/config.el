;;; ui/telephone/config.el -*- lexical-binding: t; -*-

(use-package! telephone-line
  :hook (after-init . telephone-line-mode)
  :init
                                        ; Faces
  (defface my-accent-dark    '((t (:foreground "Black" :background "Cadet Blue"))) "")
  (defface my-evil-dark      '((t (:foreground "White" :background "Black"))) "")
  (defface my-accent-light   '((t (:foreground "black" :background "Light Slate Grey"))) "")
  (defface my-evil-light     '((t (:foreground "black" :background "Sky Blue"))) "")

  ;; Set telephone line faces
  (setq telephone-line-faces
    '((evil . (my-evil-dark . my-evil-dark))
       (modal . telephone-line-modal-face)
       (ryo . telephone-line-ryo-modal-face)
       (accent . (my-accent-dark . telephone-line-accent-inactive))
       (nil mode-line . mode-line-inactive)))
  (when (featurep! +light)
    (setq telephone-line-faces
      '((evil . (my-evil-light . my-evil-light))
         (modal . telephone-line-modal-face)
         (ryo . telephone-line-ryo-modal-face)
         (accent . (my-accent-light . telephone-line-accent-inactive))
         (nil mode-line . mode-line-inactive))))

                                        ; Seperators
  (setq telephone-line-primary-left-separator    'telephone-line-abs-left
    telephone-line-secondary-left-separator  'telephone-line-identity-hollow-left
    telephone-line-primary-right-separator   'telephone-line-abs-right
    telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)

                                        ; LSP segment
  (telephone-line-defsegment +oreoline-lsp-segment ()
    (if (bound-and-true-p lsp-mode)
        (propertize "")
      (propertize "")))

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
