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

(defvar ep/pwd-max-len 30)
(defvar ep/git-branch-max-len 5)

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
        (,(ep/--pwd)
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
           (thread-last
             (propertize (car item)
                         'font-lock-face (cdr item)
                         'front-sticky   '(font-lock-face read-only)
                         'rear-nonsticky '(font-lock-face read-only))
             (if (not (listp item))
                 item)))))))

(defun ep/--colour-on-last-command ()
  "Returns an Emacs colour based on ESHELL-LAST-COMMAND-STATUS."
  (if (zerop eshell-last-command-status)
      ep/success-colour
    ep/failure-colour))

(defun ep/--with-fg-colour (s colour)
  "Helper which propertises a string `s' with foreground colour `colour'"
  (propertize s 'font-lock-face `(:foreground ,colour)))

(defun ep/--user-and-remote ()
  "If in a remote directory, return a string representing that host,
otherwise empty string."
  (if (file-remote-p default-directory)
      (let ((user (file-remote-p default-directory 'user))
            (host (file-remote-p default-directory 'host)))
        (thread-first
          (if user
              (format "%s@%s" user host)
            host)
          (concat ":")))
    ""))

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
      (concat
       (cond
        ((string= status "ahead")
         (ep/--with-fg-colour "→" ep/ahead-colour))
        ((string= status "behind")
         (ep/--with-fg-colour "←" ep/failure-colour)))
       (thread-first diff
                     1+
                     (nth branch-status))))))

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
         (command-output (thread-first
                           (shell-command-to-string git-cmd)
                           (split-string "\n")
                           butlast))
         (status-codes (mapcar #'(lambda (s) (cons (substring s 0 1) (substring s 1 2)))
                          command-output))
         (count-f (lambda (coll) (thread-first
                              (lambda (x) (not (or (string= x "?") (string= x " "))))
                              (cl-count-if
                               coll))))
         (total (length status-codes))
         (staged (funcall count-f (mapcar #'car status-codes)))
         (modified (funcall count-f (mapcar #'cdr status-codes)))
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
                        (cl-remove-if #'(lambda (s) (thread-last (length s) (= 0))))
                        (cl-find-if #'(lambda (s) (thread-last (substring s 0 1) (string= "*"))))))
         (branch-name (thread-last
                        (substring branch-name 2)
                        (if (null branch-name) nil))))
    (if branch-name
        (ep/--abbreviate-str
         (cond
          ((string= "(" (substring branch-name 0 1))
           (replace-regexp-in-string
            "\n$" ""
            (shell-command-to-string "git rev-parse --short HEAD")))
          (t branch-name))
         "-"
         ep/git-branch-max-len))))

(defun ep/--pwd ()
  (let ((pwd (thread-last (eshell/pwd)
                          tramp-file-local-name
                          abbreviate-file-name)))
    (ep/--abbreviate-str pwd "/" ep/pwd-max-len)))

(defun ep/--abbreviate-str (to-abbrev sep max-len)
  (if (<= (length to-abbrev) max-len)
      to-abbrev
    (let ((str "")
          (len (length to-abbrev))
          (components (split-string to-abbrev sep)))
      (while (and (> len max-len)
                  (cdr components))
        (let* ((comp (car components))
               (ab-comp (cond
                         ((= 0 (length comp)) "")
                         ((= 1 (length comp)) comp)
                         ((char-equal (elt comp 0) ?.)
                          (substring comp 0 3))
                         (t
                          (substring comp 0 2)))))
          (setq str (concat str ab-comp sep)
                len (- len (length ab-comp))
                components (cdr components))))
      (thread-last
        components
        (cl-reduce #'(lambda (a b) (concat a sep b)))
        (if (null components) "")
        (concat str)))))

(provide 'eshell-prompt)
;;; eshell-prompt.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ep" . "eshell-prompt"))
;; End:
