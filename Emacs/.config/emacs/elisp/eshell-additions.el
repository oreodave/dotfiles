;;; eshell-additions.el --- Some aliases for Eshell    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Aryadev Chavali

;; Author: Aryadev Chavali <aryadev@aryadevchavali.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License Version
;; 2 as published by the Free Software Foundation.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(autoload #'eshell            "eshell")
(autoload #'eshell/cd         "eshell")
(autoload #'eshell/echo       "eshell")
(autoload #'eshell/send-input "eshell")

;; General helpers
(defun eshell-goto-latest-prompt ()
  "Move point to the most recent eshell prompt and clear anything before it."
  (interactive)
  (goto-char (point-max))
  (eshell-bol)
  (delete-region (point) (point-max)))

(defun eshell-send-command (cmd)
  (interactive "sCommand: ")
  (eshell-goto-latest-prompt)
  (insert cmd)
  (eshell-send-input))

;; Aliases
(defun eshell/goto (&rest args)
  "Use `read-file-name' to change directories"
  (let* ((name (read-file-name "Choose file: " (thing-at-point 'filename t)))
         (dir (file-name-directory name)))
    (eshell-goto-latest-prompt)
    (eshell/cd (list dir))
    (if (not (file-directory-p name))
        (find-file name))))

(defun eshell/project-root (&rest args)
  "Change to directory `project-root'"
  (cond
   ((project-current)
    (eshell-goto-latest-prompt)
    (eshell/cd (list (project-root (project-current)))))
   (t
    (setq eshell-last-command-status 1)
    (eshell/echo
     (format "[%s]: No project in current directory"
             (propertize "Error" 'font-lock-face '(:foreground "red")))))))

(defun eshell/sudo-switch (&rest args)
  "Switch to and from administrative (sudo) mode in Eshell.
Uses tramp to figure out if we're in sudo mode or not.  "
  (let ((user (file-remote-p default-directory 'user)))
    (cond
     ((null user)
      (let ((wrapped-dir (concat "/sudo::" default-directory)))
        (eshell/cd wrapped-dir)))
     ((string= user "root")
      (eshell/cd (file-remote-p default-directory 'localname))))))

;; +eshell/open and +eshell/at-cwd
(defun +eshell/--current-instances ()
  (cl-loop for buffer being the buffers
           if (with-current-buffer buffer
                (eq major-mode 'eshell-mode))
           collect
           (cons (buffer-name buffer) buffer)))

(defun +eshell/--choose-instance ()
  (let* ((current-instances (+eshell/--current-instances))
         (answer (completing-read "Enter name: " (mapcar #'car current-instances)))
         (result (assoc answer current-instances)))
    (cond
     (result (switch-to-buffer (cdr result))
             (cdr result))
     ((not (string= answer ""))
      (let ((eshell-buffer-name (format "*%s-eshell*" answer)))
        (eshell nil)))
     (t
      (eshell)))))

(defun +eshell/open (&optional arg)
  "Open an instance of EShell, displaying it.

Numeric arguments passed into `+eshell/open' are reduced by 1 i.e. C-u 1
+eshell/open will map to the default eshell instance, C-u 2 +eshell/open
will map to the next labelled eshell instance, etc.

Any numeric or null argument is passed to the `eshell' function.

Otherwise, if C-u is used, you can select an instance to spawn."
  (interactive "P")
  (cond
   ((null arg) (eshell))
   ((numberp arg)
    (eshell arg))
   (t (+eshell/--choose-instance))))

(defun +eshell/at-cwd (&optional arg)
  "Open an instance of eshell at the current working directory.

Pass argument to `+eshell/open'."
  (interactive "P")
  (let ((dir (if buffer-file-name
                 (file-name-directory buffer-file-name)
               default-directory))
        (buf (+eshell/open arg)))
    (with-current-buffer buf
      (eshell/cd dir)
      (eshell-send-input))))

(provide 'eshell-additions)
;;; eshell-additions.el ends here
