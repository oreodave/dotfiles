;;; move-line.el --- Move current line up or down    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Aryadev Chavali

;; Author: Aryadev Chavali <aryadev@aryadevchavali.com>
;; Keywords:

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <https://unlicense.org>

;;; Commentary:

;; Move the current line up or down.  Shamelessly copied from
;; https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/ so

;;; Code:

(defmacro +move/create-move (name description &rest body)
  `(defun ,name (&optional arg)
     ,description
     (interactive "P")
     (let ((arg (if (or (null arg) (< arg 1)) 1 arg)))
       (while (not (= arg 0))
         ,@body
         (setq arg (- arg 1))))))

(+move/create-move
 +move/line-up
 "Move the current line up"
 (transpose-lines 1)
 (forward-line -2)
 (indent-according-to-mode))

(+move/create-move
 +move/line-down
 "Move the current line down"
 (forward-line 1)
 (transpose-lines 1)
 (forward-line -1)
 (indent-according-to-mode))

(+move/create-move
 +move/word-forward
 "Move the current word forward"
 (forward-word 1)
 (transpose-words 1)
 (forward-word -1)
 (indent-according-to-mode))

(+move/create-move
 +move/word-backward
 "Move the current word backward"
 (transpose-words 1)
 (forward-word -2)
 (indent-according-to-mode))

(provide 'move)
;;; move.el ends here
