;;; private/completion/config.el -*- lexical-binding: t; -*-

(use-package! ido
  :hook (doom-first-input . ido-mode)
  :hook (ido-mode . recentf-mode)
  :hook (ido-mode . ido-everywhere)
  :hook (ido-mode . ido-ubiquitous-mode)

  :preface
  (defadvice! +ido-run-hooks-a (&rest _)
    :after #'ido-mode
    (run-hooks 'ido-mode-hook))

  :init
  (setq ido-save-directory-list-file nil)
  (setq ido-save-directory-list-file nil)
  (setq ido-ignore-buffers '("\\` " "^\\*ESS\\*" "^\\*[Hh]elp" "^\\*.*Completions\\*$" "^\\*tramp" "^\\*cvs-" "^*Ido"))
  (setq ido-separator "\n")

  :config
  (defun +completion/recentf()
    (interactive)
    (find-file (completing-read "Recentf: " recentf-list)))

  (define-key!
    [remap recentf-open-files] #'+completion/recentf
    [remap completing-read]    #'ido-completing-read)

  (map!
   :map (ido-common-completion-map ido-file-completion-map ido-buffer-completion-map)
   "C-k" #'ido-prev-match
   "C-j" #'ido-next-match
   "TAB" #'ido-exit-minibuffer
   :map ido-file-completion-map
   "~" (cmd! (if (looking-back "/" (point-min))
                 (insert "~/")
               (call-interactively #'self-insert-command)))))


(use-package! ido-sort-mtime
  :hook (ido-mode . ido-sort-mtime-mode))


(use-package! crm-custom
  :hook (ido-mode . crm-custom-mode))


(use-package! flx-ido
  :hook (ido-mode . flx-ido-mode))
