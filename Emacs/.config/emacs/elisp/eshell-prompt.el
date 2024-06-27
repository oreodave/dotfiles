;;; eshell-prompt.el --- Generating a good prompt for Eshell  -*- lexical-binding: t; -*-

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

;; We provide a function +eshell-prompt which generates a prompt on
;; demand.

;;; Code:

(defvar +eshell-prompt/user-prompt "𝜆> "
  "Prompt for user to input.")

(defun +eshell-prompt/--colour-on-last-command ()
  "Returns an Emacs colour based on ESHELL-LAST-COMMAND-STATUS."
  (if (zerop eshell-last-command-status)
      "forestgreen"
    "darkred"))

(defun +eshell-prompt/--git-remote-status ()
  "Returns a propertized string for the status of a repository
in comparison to its remote.  3 differing strings are returned
dependent on:

- Is it equivalent to the remote?
- Is it ahead of the remote?
- Is it behind the remote?

The latter 2 also have a number for exactly how many commits
behind or ahead the local repository is."
  (let* ((git-cmd "git status | grep 'Your branch is'")
         (branch-status (split-string (shell-command-to-string git-cmd)))
         (status (nth 3 branch-status))
         (diff (cl-position "by" branch-status :test #'string=)))
    (if (null diff)
        (propertize "=" 'font-lock-face '(:foreground "green"))
      (let ((n (nth (+ 1 diff) branch-status)))
        (concat
         (cond
          ((string= status "ahead")
           (propertize "→" 'font-lock-face '(:foreground "dodger blue")))
          ((string= status "behind")
           (propertize "←" 'font-lock-face '(:foreground "orange red"))))
         n)))))

(defun +eshell-prompt/--git-change-status ()
  "Returns a propertized string for the condition of the worktree in
a repository.  If there are no changes i.e. the worktree is clean
then a green tick is returned, but if there are changes then the
number of files affected are returned in red."
  (let* ((git-cmd "git status -s")
         (command-output (split-string (shell-command-to-string git-cmd) "\n"))
         (changed-files (- (length command-output) 1)))
    (if (= changed-files 0)
        (propertize "✓" 'font-lock-face '(:foreground "green"))
      (propertize (number-to-string changed-files) 'font-lock-face '(:foreground "red")))))

(defun +eshell-prompt/--git-status ()
  "Returns a completely formatted string of
form (BRANCH-NAME<CHANGES>[REMOTE-STATUS])."
  (let ((git-branch (shell-command-to-string "git branch")))
    (if (or (string= git-branch "")
           (not (string= "*" (substring git-branch 0 1))))
        ""
      (format
       "(%s<%s>[%s])"
       (nth 2 (split-string git-branch "\n\\|\\*\\| "))
       (+eshell-prompt/--git-change-status)
       (+eshell-prompt/--git-remote-status)))))

(defun +eshell-prompt/make-prompt ()
  (let ((git (+eshell-prompt/--git-status)))
    (mapconcat
     (lambda (item)
       (if (listp item)
           (propertize (car item)
                       'read-only t
                       'font-lock-face (cdr item)
                       'front-sticky   '(font-lock-face read-only)
                       'rear-nonsticky '(font-lock-face read-only))
         item))
     (list
      "["
      `(,(abbreviate-file-name (eshell/pwd)) :foreground "LimeGreen")
      "]"
      (if (string= git "")
          ""
        (concat "-" git ""))
      "\n"
      `(,(format-time-string "[%H:%M:%S]") :foreground "purple")
      "\n"
      (list "𝜆> " ':foreground (+eshell-prompt/--colour-on-last-command))))))


(provide 'eshell-prompt)
;;; eshell-prompt.el ends here
