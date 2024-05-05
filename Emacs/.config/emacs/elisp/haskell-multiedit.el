;;; haskell-multiedit.el --- Minor mode to edit and evaluate
;;; multi-line scripts of Haskell without making a file
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Aryadev Chavali

;; Author: Aryadev Chavali <aryadev@aryadevchavali.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; To be used in a haskell-interactive-mode buffer.  It'll generate a
;; new buffer with haskell-mode and this minor mode activated.  Once
;; finished with the code, using another keybind to close the buffer
;; and paste the code into haskell-interactive-mode, evaluating it all
;; line by line with indenting and multi-line guards.

;;; Code:

(defvar haskell-multiedit-mode-map
  (make-sparse-keymap))

(define-minor-mode haskell-multiedit-mode
  "Minor mode for haskell buffers generated by haskell-interactive-mode.
Provides bindings to retrieve content back into the REPL with
full multi-edit commands"
  :lighter nil
  :keymap haskell-multiedit-mode-map)

(with-eval-after-load "haskell"
  (defun haskell-multiedit ()
    (interactive)
    (haskell-interactive-mode-return)
    (switch-to-buffer "*haskell-temp*")
    (haskell-mode)
    (haskell-multiedit-mode))

  (defun haskell-multiedit-evaluate-at-repl ()
    (interactive)
    (let* ((contents (buffer-string))
           (lines (split-string contents "\n")))
      (switch-to-buffer (haskell-session-interactive-buffer (haskell-session)))
      (insert ":{")
      (haskell-interactive-mode-return)
      (mapc #'(lambda (line) (insert line) (haskell-interactive-mode-return)) lines)
      (insert ":}")
      (haskell-interactive-mode-return)))

  (define-key haskell-interactive-mode-map (kbd "C-c '") #'haskell-multiedit)
  (define-key haskell-multiedit-mode-map (kbd "C-c '") #'haskell-multiedit-evaluate-at-repl))

(provide 'haskell-multiedit)
;;; haskell-interactive-multiedit.el ends here
