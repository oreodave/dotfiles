;;; org-bookmark.el --- Bookmark manager using org-mode  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'org)

(defvar +bookmark/file (expand-file-name (concat org-directory "/bookmarks.org")))
(defvar +bookmark/mpv-args "--ytdl-raw-options=force-ipv4= --ytdl-format=22 -v")

(defun +bookmark/--extract-heading ()
  (let ((heading-components (org-heading-components))
        (tags (org-get-tags)))
    (message "%s" tags)
    (list
     (nth 4 heading-components)
     (cl-remove-if #'(lambda (tag) (string= tag "bookmark")) tags)
     (substring-no-properties
      (org-agenda-get-some-entry-text
       (point-marker)
       most-positive-fixnum)))))

(defun +bookmark/--extract-all-heading-data ()
  (cl-remove-if
   #'(lambda (x) (member "DONE" (nth 1 x)))
   (org-scan-tags
    #'+bookmark/--extract-heading
    (cdr (org-make-tags-matcher ":bookmark:"))
    nil)))

(defun +bookmark/--heading->record (heading)
  (cl-destructuring-bind (name tags url) heading
    (cons
     (concat name
             " "
             (substring-no-properties
              (cl-reduce #'(lambda (x y) (concat x ":" y))
                         tags
                         :initial-value "")))
     url)))

(defun +bookmark/bookmarks ()
  (with-current-buffer (find-file-noselect +bookmark/file)
    (mapcar
     #'+bookmark/--heading->record
     (+bookmark/--extract-all-heading-data))))

(defun +bookmark/open-bookmark ()
  (interactive)
  (let* ((bookmarks (+bookmark/bookmarks))
         (choice (completing-read "Choose bookmark: "
                                  (mapcar #'car bookmarks)
                                  nil t))
         (pair (assoc choice bookmarks #'string=)))
    (if (null pair)
        (error (format "`%s' is not a valid bookmark" choice))
      (message "[bookmark]: Opening `%s`" (car pair))
      (cond
       ((or
         (string-match-p "^https://\\(www.\\)?youtu\\(.\\)?be" (cdr pair))
         (string-match-p "\\.mp4$" (cdr pair)))
        ;; Open MPV
        (message "[bookmark]: Starting MPV process")
        (with-current-buffer (get-buffer-create "*mpv*")
          (ansi-color-for-comint-mode-on)
          (comint-mode))
        (set-process-filter (start-process-shell-command
                             "bookmark-mpv" "*mpv*"
                             (concat "mpv " +bookmark/mpv-args " \"" (cdr pair) "\""))
                            #'comint-output-filter))
       (t
        (message "[bookmark]: Starting eww")
        (eww (cdr pair)))))))

(provide 'org-bookmark)
;;; bookmark.el ends here
