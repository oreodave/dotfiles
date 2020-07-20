;;; private/mail/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +mail/open-mail ()
  "Activate (or switch to) `notmuch' in its workspace."
  (interactive)
  (unless (featurep! :ui workspaces)
    (user-error ":ui workspaces is required, but disabled"))
  (condition-case-unless-debug e
      (progn
        (+workspace-switch "*MAIL*" t)
        (if-let* ((buf (cl-find-if (lambda (it) (string-match-p "^\\*notmuch" (buffer-name (window-buffer it))))
                                   (doom-visible-windows))))
            (select-window (get-buffer-window buf))
          (notmuch-search "tag:inbox"))
        (+workspace/display))
    ('error
     (+notmuch/quit)
     (signal (car e) (cdr e)))))
