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
(defvar ep/success-colour "green2")
(defvar ep/failure-colour "red")
(defvar ep/branch-name-colour "LightSalmon")
(defvar ep/pipe-colour "green4")
(defvar ep/ahead-colour "dodger blue")
(defvar ep/remote-colour "DarkGoldenrod")

(defun ep/make-prompt ()
  (let ((git (ep/--git-status)))
    (thread-last
      `(("┌──" :foreground ,ep/pipe-colour)
        "["
        (,(ep/--user-and-remote) :foreground ,ep/remote-colour)
        (,(abbreviate-file-name (tramp-file-local-name (eshell/pwd)))
         :foreground ,ep/dir-colour)
        ,(if (string= git "")
             ""
           (concat "]─[" git))
        "]"
        "\n"
        ("└─>" :foreground ,ep/pipe-colour)
        (,ep/user-prompt :foreground ,(ep/--colour-on-last-command)))
      (mapconcat
       #'(lambda (item)
           (if (listp item)
               (propertize (car item)
                           'font-lock-face (cdr item)
                           'front-sticky   '(font-lock-face read-only)
                           'rear-nonsticky '(font-lock-face read-only))
             item))))))

(defun ep/--with-fg-colour (s colour)
  "Helper which propertises a string `s' with foreground colour `colour'"
  (propertize s 'font-lock-face `(:foreground ,colour)))

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
        (ep/--with-fg-colour "=" ep/success-colour)
      (let ((n (nth (+ 1 diff) branch-status)))
        (concat
         (cond
          ((string= status "ahead")
           (ep/--with-fg-colour "→" ep/ahead-colour))
          ((string= status "behind")
           (ep/--with-fg-colour "←" ep/failure-colour)))
         n)))))

(defun ep/--git-change-status ()
  "Returns a propertized string for the condition of the worktree in
a repository.

If there are no changes i.e. the worktree is clean then a green tick is
returned.

If there are changes then we characterise it by the following parameters:
- staged changes in green
- unstaged but tracked changes in blue
- untracked files in red
"
  (let* ((git-cmd "git status -s")
         (command-output
          (thread-first (shell-command-to-string git-cmd)
                        (split-string "\n")
                        butlast))
         (status-codes (mapcar #'(lambda (s) (cons (substring s 0 1) (substring s 1 2)))
                          command-output))
         (filter-f (lambda (x) (not (or (string= x "?") (string= x " ")))))
         (total (length status-codes))
         (staged (cl-count-if (lambda (x) (funcall filter-f (car x))) status-codes))
         (modified (cl-count-if (lambda (x) (funcall filter-f (cdr x))) status-codes))
         (not-tracked (cl-count-if (lambda (x) (string= (cdr x) "?")) status-codes)))
    (if (= total 0)
        (ep/--with-fg-colour "✓" ep/success-colour)
      (thread-last
        (list
         (ep/--with-fg-colour (number-to-string staged) ep/success-colour)
         (ep/--with-fg-colour (number-to-string modified) ep/ahead-colour)
         (ep/--with-fg-colour (number-to-string not-tracked) ep/failure-colour))
        (cl-remove-if #'(lambda (s) (string= s "0")))
        (mapconcat #'(lambda (s) (concat s "/")))))))

(defun ep/--git-branch-name ()
  "Get the branch name of the current working directory.

If a deteached head, return the SHA."
  (let* ((branch-name (thread-last
                        (split-string (shell-command-to-string "git branch") "\n")
                        (cl-remove-if #'(lambda (s) (= (length s) 0)))
                        (cl-find-if #'(lambda (s) (string= "*" (substring s 0 1))))))
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
  "Returns a completely formatted string of form
BRANCH-NAME(REMOTE-STATUS)(CHANGES)."
  (let ((git-branch (ep/--git-branch-name)))
    (if (null git-branch)
        ""
      (format
       "%s(%s)(%s)"
       (ep/--with-fg-colour git-branch ep/branch-name-colour)
       (ep/--git-remote-status)
       (ep/--git-change-status)))))

(defun ep/--user-and-remote ()
  "If in a remote directory, return a string representing that host,
otherwise empty string."
  (if (file-remote-p default-directory)
      (let ((user (file-remote-p default-directory 'user))
            (host (file-remote-p default-directory 'host)))
        (concat
         (if user
             (format "%s@%s" user host)
           host)
         ":"))
    ""))

(provide 'eshell-prompt)
;;; eshell-prompt.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ep" . "eshell-prompt"))
;; End:
