;;; better-mode-line.el --- A better mode-line system designed by me  -*- lexical-binding: t; -*-

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

;; There are 3 lists for the left, centre and right of the mode-line.  They're
;; the same as mode-line-format in terms of general schema as they're fed
;; directly into that variable.  Padding strings are automatically generated
;; based on these segments so be sure to set these.

;;; Code:

(defvar bml/left-segment nil
  "List of elements that are placed on the left of the mode-line")

(defvar bml/centre-segment nil
  "List of elements that should be on the centre of the mode-line")

(defvar bml/right-segment nil
  "List of elements that should be on the right of the mode-line")

(defvar bml/--minimum-padding 4
  "Minimum size of padding string.")

(defun bml/--get-padding-size (other-size)
  "Compute length of padding to ensure string of size OTHER is on an
extreme end to CENTRE-SEGMENT."
  (let* ((centre-size (length (format-mode-line bml/centre-segment)))
         (win-width (window-width))
         (margins (window-margins))
         (width (if (null (car margins))
                    win-width
                  (+ (car margins) win-width (cdr margins)))))
    (- (floor (/ width 2)) (floor (/ centre-size 2)) other-size)))

(defun bml/--generate-padding (segment)
  "Make padding string to separate center segment from SEGMENT."
  (let* ((segment-size (length (format-mode-line segment)))
         (padding-size (bml/--get-padding-size segment-size)))
    (make-string (if (< padding-size bml/--minimum-padding)
                     bml/--minimum-padding
                   padding-size)
                 ?\s)))

(defun bml/setup-mode-line ()
  "Call this to setup the mode-line when:
- first loading the package.
- segments are updated."
  (setq-default mode-line-format
                `(,bml/left-segment
                  (:eval (bml/--generate-padding
                          bml/left-segment))
                  ,bml/centre-segment
                  ;; NOTE: Emacs 30!
                  mode-line-format-right-align
                  ,bml/right-segment)))

(provide 'better-mode-line)

;;; better-mode-line.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("bml" . "better-mode-line"))
;; End:
