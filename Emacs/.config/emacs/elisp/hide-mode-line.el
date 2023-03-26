;;; hide-mode-line.el --- Hide the modeline super easy  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Aryadev Chavali

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

;; Defines a minor mode which toggles off the mode line.

;;; Code:

(defvar hide-mode-line--prev-mode-line nil)

(define-minor-mode
  hide-mode-line-mode
  "Hides the mode line."
  :lighter nil
  (if mode-line-format
      (progn
        (setq-local hide-mode-line--prev-mode-line mode-line-format)
        (setq-local mode-line-format nil))
      (setq-local mode-line-format hide-mode-line--prev-mode-line)))


(provide 'hide-mode-line)
;;; hide-mode-line.el ends here
