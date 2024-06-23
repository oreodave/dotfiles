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

(autoload #'eshell/cd "eshell")
(autoload #'eshell/echo "eshell")
(autoload #'eshell/send-input "eshell")

;; Aliases
(defun eshell/goto (&rest args)
  "Use `read-directory-name' to change directories"
  (eshell/cd (list (read-directory-name "Directory?: "))))

(defun eshell/project-root (&rest args)
  "Change to directory `project-root'"
  (if (project-current)
      (eshell/cd (list (project-root (project-current))))
    (let ((error-msg (propertize "Error" 'font-lock-face
                                 '(:foreground "red"))))
      (eshell/echo
       (format "[%s]: No project in current directory" error-msg)))))

;; Additional functions
(defun +eshell/at-cwd ()
  "Open an instance of eshell at the current working directory."
  (interactive)
  (let ((dir (if buffer-file-name
                 (file-name-directory buffer-file-name)
               default-directory))
        (buf (eshell)))
    (with-current-buffer buf
      (eshell/cd dir)
      (eshell-send-input))))

(defun +eshell/--current-instances ()
  (cl-loop for buffer being the buffers
           if (with-current-buffer buffer
                (eq major-mode 'eshell-mode))
           collect
           (cons (buffer-name buffer) buffer)))

(defun +eshell/open (&optional ARG)
  "If no arg is given, run EShell as per usual.
If an arg is given, then interactively open a new Eshell instance
or a currently opened one, naming it in the process."
  (interactive "P")
  (if (null ARG)
      (eshell)
    (let* ((current-instances (+eshell/--current-instances))
           (answer (completing-read "Enter name: " (mapcar #'car current-instances)))
           (result (assoc answer current-instances)))
      (cond
       (result (switch-to-buffer (cdr result)))
       ((not (string= answer ""))
        (let ((eshell-buffer-name (format "*%s-eshell*" answer)))
          (eshell)))
       (t
        (eshell))))))

(provide 'eshell-additions)
;;; eshell-additions.el ends here
