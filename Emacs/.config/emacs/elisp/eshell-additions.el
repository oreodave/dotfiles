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

;; Aliases
(defun eshell/goto (&rest args)
  "Use `read-directory-name' to change directories"
  (let* ((name (read-file-name "Choose file: "))
         (dir (file-name-directory name)))
    (eshell/cd (list dir))
    (if (not (file-directory-p name))
        (find-file name))))

(defun eshell/project-root (&rest args)
  "Change to directory `project-root'"
  (if (project-current)
      (eshell/cd (list (project-root (project-current))))
    (eshell/echo
     (format "[%s]: No project in current directory"
             (propertize "Error" 'font-lock-face '(:foreground "red"))))))

(defun eshell/sudo-switch (&rest args)
  "Switch to a tramp connection sudo in the current directory"
  (let ((wrapped-dir (concat "/sudo::" default-directory)))
    (eshell/cd wrapped-dir)))

;; Additional functions
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

(defun +eshell/--current-instances ()
  (cl-loop for buffer being the buffers
           if (with-current-buffer buffer
                (eq major-mode 'eshell-mode))
           collect
           (cons (buffer-name buffer) buffer)))

(defun +eshell/open (&optional arg)
  "Open an instance of EShell, displaying it.

If there exists only one instance of EShell, display it.  Otherwise, prompt with
a list of open instances.  If user selects an instance, display that instance.
Otherwise, create an instance with the name given.

If `arg' is non nil, then always prompt user to select an instance."
  (interactive "P")
  (let ((current-instances (+eshell/--current-instances)))
    (cond
     ((and (null current-instances)
           (null arg))
      (eshell))
     ((and (= (length current-instances) 1)
           (null arg))
      (switch-to-buffer (cdar current-instances)))
     (t
      (let* ((answer (completing-read "Enter name: " (mapcar #'car current-instances)))
             (result (assoc answer current-instances)))
        (cond
         (result (switch-to-buffer (cdr result)))
         ((not (string= answer ""))
          (let ((eshell-buffer-name (format "*%s-eshell*" answer)))
            (eshell nil)))
         (t
          (eshell))))))))

(provide 'eshell-additions)
;;; eshell-additions.el ends here
