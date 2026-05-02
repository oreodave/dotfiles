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

;; This package automates the configuration of `mode-line-format'.  Usage is:
;; 1) Setting `bml/left-segment', `bml/center-segment', and `bml/right-segment'
;; 2) Calling `bml/setup-mode-line'.
;; These segments are composed together and padded such that they are placed
;; appropriately.
;; I recommend looking at [[file:../config.org::*Better Mode line][Better Mode
;; line]] for an example of using this package.


;;; Code:

(defvar bml/left-segment nil
  "List of elements that are placed on the left of the mode-line")

(defvar bml/center-segment nil
  "List of elements that should be on the center of the mode-line")

(defvar bml/right-segment nil
  "List of elements that should be on the right of the mode-line")

(defvar bml/--minimum-padding 4
  "Minimum size of padding string.")

(defun bml/--generate-left-padding ()
  "Make padding string to separate the left and center segments."
  (let* ((left-size (length (format-mode-line bml/left-segment)))
         (center-size (length (format-mode-line bml/center-segment)))
         (window-width
          (+ (or (car (window-margins)) 0) ; left margin, or 0.
             (window-width)))
         (padding-size (floor (- (/ window-width 2) (/ center-size 2) left-size))))
    (make-string (max padding-size bml/--minimum-padding) ?\s)))

(defun bml/setup-mode-line ()
  "Call this to setup the mode-line.
As this sets `mode-line-format', use this function:
- when utilising the package
- any segments are updated"
  (thread-last
    `(,bml/left-segment
      (:eval (bml/--generate-left-padding))
      ,bml/center-segment
      ;; NOTE: Emacs 30!
      mode-line-format-right-align
      ,bml/right-segment)
    (setq-default mode-line-format)))

(provide 'better-mode-line)

;;; better-mode-line.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("bml" . "better-mode-line"))
;; End:
