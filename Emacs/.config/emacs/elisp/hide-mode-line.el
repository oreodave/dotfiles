;;; hide-mode-line.el --- Hide the modeline super easy  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Aryadev Chavali

;; Author: Aryadev Chavali <aryadev@aryadevchavali.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License version
;; 2 as published by the Free Software Foundation.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Defines a minor mode which toggles off the mode line.

;;; Code:

(defvar hide-mode-line--prev-mode-line nil)

(define-minor-mode hide-mode-line-mode
  "Minor mode for hiding model lines"
  :lighter nil
  (cond
   ((and mode-line-format hide-mode-line-mode)
    (setq-local hide-mode-line-mode t
                hide-mode-line--prev-mode-line mode-line-format
                mode-line-format nil))
   (t (setq-local hide-mode-line nil
                  mode-line-format hide-mode-line--prev-mode-line
                  hide-mode-line--prev-mode-line nil))))

(define-globalized-minor-mode global-hide-mode-line-mode hide-mode-line-mode
  (lambda nil (hide-mode-line-mode t)))

(provide 'hide-mode-line)
;;; hide-mode-line.el ends here
