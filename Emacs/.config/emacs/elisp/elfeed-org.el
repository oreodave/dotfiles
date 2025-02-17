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
  (let ((title-context (org-element-context)))
    (org-element-property :raw-link title-context)))

(defun elfeed-org/--parse-tags ()
  (mapcar #'intern (org-get-tags)))

(defun elfeed-org/--parse-headline ()
  (if-let* ((ctx (org-element-context))
            (link (elfeed-org/--parse-link ctx))
            (tags (elfeed-org/--parse-tags)))
      (cons link tags)
    nil))

(defun elfeed-org/--parse-headlines ()
  (cl-remove-if
   #'null
   (org-map-entries #'elfeed-org/--parse-headline t)))

(defun elfeed-org ()
  (setq elfeed-feeds
        (with-current-buffer (find-file-noselect elfeed-org/file)
          (elfeed-org/--parse-headlines))))

(provide 'elfeed-org)
;;; elfeed-org.el ends here
