;;; private/startup/config.el -*- lexical-binding: t; -*-

(defun +startup/create-scratch-message ()
  "Generate a string for the scratch buffer"
  (format "Welcome to Emacs! (｡◕‿◕｡)
Load time was %s
Time of startup: %s"
          (emacs-init-time)
          (current-time-string (current-time))))

(add-hook 'doom-first-input-hook
          #'(lambda ()
              (setq-default mode-line-format (list "%l:%c %P \t %+%b(" '(:eval (format "%s" major-mode)) ") \t %I \t" vc-mode mode-line-end-spaces))
              (setq-default initial-scratch-message (+startup/create-scratch-message))))
