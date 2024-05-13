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

(defvar +bookmark/file (expand-file-name "~/Text/bookmarks.org"))
(defvar +bookmark/mpv-args "--ytdl-raw-options=force-ipv4= --ytdl-format=22")

(defun +bookmark/bookmarks ()
  (with-current-buffer (find-file-noselect +bookmark/file)
    (org-scan-tags
     #'(lambda nil
         (let ((heading-components (org-heading-components)))
           (cons
            (concat (nth 4 heading-components) (nth 5 heading-components))
            (substring-no-properties (org-agenda-get-some-entry-text
                                      (point-marker)
                                      most-positive-fixnum)))))
     (cdr (org-make-tags-matcher ":bookmark:"))
     nil)))

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
                             (concat
                              "mpv "
                              +bookmark/mpv-args
                              " \""
                              (cdr pair)
                              "\""))
                            #'comint-output-filter))
       (t
        (message "[bookmark]: Starting eww")
        (eww (cdr pair)))))))

(provide 'org-bookmark)
;;; bookmark.el ends here
