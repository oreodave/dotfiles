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

;; We provide a function ep which generates a prompt on
;; demand.

;;; Code:

(defvar ep/user-prompt " λ "
  "Prompt for user to input.")

(defvar ep/dir-colour "deepskyblue")
(defvar ep/success-colour "forestgreen")
(defvar ep/failure-colour "red")
(defvar ep/branch-name-colour "LightSalmon")
(defvar ep/pipe-colour "green2")
(defvar ep/ahead-colour "dodger blue")
(defvar ep/remote-colour "DarkGoldenrod")

(defun ep/--colour-on-last-command ()
  "Returns an Emacs colour based on ESHELL-LAST-COMMAND-STATUS."
  (if (zerop eshell-last-command-status)
      ep/success-colour
    ep/failure-colour))

(defun ep/--git-remote-status ()
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
        (propertize "=" 'font-lock-face `(:foreground ,ep/success-colour))
      (let ((n (nth (+ 1 diff) branch-status)))
        (concat
         (cond
          ((string= status "ahead")
           (propertize "→" 'font-lock-face `(:foreground ,ep/ahead-colour)))
          ((string= status "behind")
           (propertize "←" 'font-lock-face `(:foreground ,ep/failure-colour))))
         n)))))

(defun ep/--git-change-status ()
  "Returns a propertized string for the condition of the worktree in
a repository.  If there are no changes i.e. the worktree is clean
then a green tick is returned, but if there are changes then the
number of files affected are returned in red."
  (let* ((git-cmd "git status -s")
         (command-output (split-string (shell-command-to-string git-cmd) "\n"))
         (changed-files (- (length command-output) 1)))
    (if (= changed-files 0)
        (propertize "✓"
                    'font-lock-face
                    `(:foreground ,ep/success-colour))
      (propertize (number-to-string changed-files)
                  'font-lock-face
                  `(:foreground ,ep/failure-colour)))))

(defun ep/--git-branch-name ()
  "Get the branch name of the current working directory.  W"
  (let* ((branch-name (thread-last
                        (split-string (shell-command-to-string "git branch") "\n")
                        (cl-remove-if (lambda (s) (= (length s) 0)))
                        (cl-find-if (lambda (s) (string= "*" (substring s 0 1))))))
         (branch-name (if (null branch-name) nil
                        (substring branch-name 2))))
    (cond
     ((null branch-name) nil)
     ((string= "(" (substring branch-name 0 1))
      (replace-regexp-in-string
       "\n$" ""
       (shell-command-to-string "git rev-parse --short HEAD")))
     (t branch-name))))

(defun ep/--git-status ()
  "Returns a completely formatted string of
  form (BRANCH-NAME<CHANGES>[REMOTE-STATUS])."
  (let ((git-branch (ep/--git-branch-name)))
    (if (null git-branch)
        ""
      (format
       "%s(%s)(%s)"
       (propertize git-branch 'font-lock-face `(:foreground ,ep/branch-name-colour))
       (ep/--git-remote-status)
       (ep/--git-change-status)))))

(defun ep/--user-and-remote ()
  (if (file-remote-p default-directory)
      (let ((user (file-remote-p default-directory 'user))
            (host (file-remote-p default-directory 'host)))
        (if user
            (format "%s@%s " user host)
          (concat host " ")))
    ""))

(defun ep/make-prompt ()
  (let ((git (ep/--git-status)))
    (mapconcat
     (lambda (item)
       (if (listp item)
           (propertize (car item)
                       'font-lock-face (cdr item)
                       'front-sticky   '(font-lock-face read-only)
                       'rear-nonsticky '(font-lock-face read-only))
         item))
     (list
      `("┌──"
        :foreground ,ep/pipe-colour)
      "["
      `(,(ep/--user-and-remote)
        :foreground ,ep/remote-colour)
      `(,(abbreviate-file-name (tramp-file-local-name (eshell/pwd)))
        :foreground ,ep/dir-colour)
      (if (string= git "")
          ""
        (concat "]─[" git))
      "]"
      "\n"
      `("└─>"
        :foreground ,ep/pipe-colour)
      (list ep/user-prompt ':foreground (ep/--colour-on-last-command))))))


(provide 'eshell-prompt)
;;; eshell-prompt.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ep" . "+eshell-prompt"))
;; End:
