;;; better-mode-line.el --- A better mode-line system designed by me  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Aryadev Chavali

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

;; There are 3 lists for the left, centre and right of the mode-line.  They're
;; the same as mode-line-format in terms of general schema as they're fed
;; directly into that variable.  Padding strings are automatically generated
;; based on these segments so be sure to set these.

;;; Code:

(defvar +better-mode-line/left-segment nil
  "List of elements that are placed on the left of the mode-line")

(defvar +better-mode-line/centre-segment nil
  "List of elements that should be on the centre of the mode-line")

(defvar +better-mode-line/right-segment nil
  "List of elements that should be on the right of the mode-line")

(defconst +better-mode-line/--minimum-padding 4
  "Minimum size of padding string.")

(defun +better-mode-line/evil-state ()
  "Returns either the empty string if no evil-state is defined or
the first character of the evil state capitalised"
  (with-eval-after-load "evil"
    (if (bound-and-true-p evil-state)
        (upcase
         (substring
	        (format "%s"
			            evil-state)
          0 1))
      "")))

(defun +better-mode-line/--get-padding-size (centre-size other-size)
  (let* ((win-width (window-width))
         (margins (window-margins))
         (width (if (null (car margins))
                    win-width
                  (+ (car margins) win-width (cdr margins)))))
    (- (/ width 2) (/ centre-size 2) other-size)))

(defun +better-mode-line/--left->centre-padding ()
  "Returns a string which pads the centre segment perfectly relative
to the left segment."
  (let* ((left-segment-size (length (format-mode-line +better-mode-line/left-segment)))
         (centre-segment-size (length (format-mode-line +better-mode-line/centre-segment)))
         (padding-size (+better-mode-line/--get-padding-size centre-segment-size left-segment-size)))
    (make-string (if (< padding-size +better-mode-line/--minimum-padding)
                     +better-mode-line/--minimum-padding
                   padding-size)
                 ?\s)))

(defun +better-mode-line/--centre->right-padding ()
  "Returns a string which pads the right segment perfectly relative
to the centre segment"
  (let* ((centre-segment-size (length (format-mode-line +better-mode-line/centre-segment)))
         (right-segment-size (length (format-mode-line +better-mode-line/right-segment)))
         (padding-size (+better-mode-line/--get-padding-size centre-segment-size right-segment-size)))
    (make-string (if (< padding-size +better-mode-line/--minimum-padding)
                     +better-mode-line/--minimum-padding
                   padding-size)
                 ?\s)))


(defun +better-mode-line/setup-mode-line ()
  "Call this to setup the mode-line when:
- first loading the package.
- segments are updated."
  (setq-default mode-line-format
                `(,@+better-mode-line/left-segment
                  (:eval (+better-mode-line/--left->centre-padding))
                  ,@+better-mode-line/centre-segment
                  (:eval (+better-mode-line/--centre->right-padding))
                  ,@+better-mode-line/right-segment)))

(provide 'better-mode-line)
;;; better-mode-line.el ends here
