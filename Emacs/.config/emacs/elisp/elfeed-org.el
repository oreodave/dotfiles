;;; elfeed-org.el --- Org integration with elfeed    -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aryadev Chavali

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

;;

;;; Code:

(defvar elfeed-org/file nil)

(defun elfeed-org/--parse-link (context)
  (thread-last (org-element-property :title context)
               search-forward)
  (org-element-property :raw-link (org-element-context)))

(defun elfeed-org/--parse-tags ()
  (thread-last
    (org-get-tags)
    (mapcar #'intern)))

(defun elfeed-org/--parse-headline ()
  (if-let* ((ctx (org-element-context))
            (link (elfeed-org/--parse-link ctx))
            (tags (elfeed-org/--parse-tags)))
      (cons link tags)
    nil))

(defun elfeed-org/--parse-headlines ()
  (thread-last
    (org-map-entries #'elfeed-org/--parse-headline t)
    (cl-remove-if #'null)))

(defun elfeed-org ()
  (thread-last
    (elfeed-org/--parse-headlines)
    (with-current-buffer (find-file-noselect elfeed-org/file))
    (setq elfeed-feeds)))

(provide 'elfeed-org)
;;; elfeed-org.el ends here
