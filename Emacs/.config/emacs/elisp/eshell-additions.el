;;; eshell-additions.el --- Some aliases for Eshell    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Aryadev Chavali

;; Author: Aryadev Chavali <aryadev@aryadevchavali.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation version 2 of the License

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

(provide 'eshell-additions)
;;; eshell-additions.el ends here
