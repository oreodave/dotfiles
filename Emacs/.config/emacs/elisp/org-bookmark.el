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

(autoload #'org-heading-components "org")
(autoload #'org-get-tags "org")
(autoload #'org-entry-get "org")
(autoload #'org-make-tags-matcher "org")

(defvar +bookmark/file (expand-file-name (concat org-directory "/bookmarks.org")))
(defvar +bookmark/mpv-args "--ytdl-raw-options=force-ipv4= --ytdl-format=22 -v")

(defun +bookmark/--get-heading-data ()
  "In an org-mode buffer, with point on a heading: get the title,
tags and url."
  (let ((heading-components (org-heading-components))
        (tags (org-get-tags))
        (url (org-entry-get (point-marker) "URL")))
    (list
     (nth 4 heading-components)
     (cl-remove-if #'(lambda (tag) (string= tag "bookmark")) tags)
     url)))

(defun +bookmark/--get-all-heading-data ()
  "In an org-mode buffer, get all the heading data (titles, tags and
urls)."
  (cl-remove-if
   #'(lambda (x) (member "DONE" (nth 1 x)))
   ;; Extract all headings with :bookmark: tag
   (org-scan-tags
    #'+bookmark/--get-heading-data
    (cdr (org-make-tags-matcher ":bookmark:"))
    nil)))

(defun +bookmark/--format-heading-data (data)
  "Format the heading data extracted into a pair of (TITLE+TAG
. URL)."
  (cl-destructuring-bind (name tags url) data
    (cons
     (format "%s %s" name
             (substring-no-properties
              (cl-reduce #'(lambda (x y) (concat x ":" y))
                         tags
                         :initial-value "")))
     url)))

(defvar +bookmark/--cache nil
  "Cached alist constructed from bookmarks file of form (TITLE+TAG
. URL).")
(defvar +bookmark/--cache-last-modified nil
  "Last modified time for bookmarks file as a float.")

(defun +bookmark/bookmarks ()
  "Get all bookmarks from +BOOKMARK/FILE in alist format.  Results
are cached for faster lookup."
  (with-current-buffer (find-file-noselect +bookmark/file)
    (let ((cur-last-modified (float-time (visited-file-modtime))))
      ;; If no cache, or no last-modified or the file has been modified
      ;; since we've last cached then recache.
      (when (or (null +bookmark/--cache)
               (null +bookmark/--cache-last-modified)
               (not (= cur-last-modified +bookmark/--cache-last-modified)))
        (setq +bookmark/--cache-last-modified cur-last-modified
              +bookmark/--cache (mapcar
                                 #'+bookmark/--format-heading-data
                                 (+bookmark/--get-all-heading-data))))))
  +bookmark/--cache)

(defun +bookmark/open-mpv (url)
  (interactive)
  (message "[bookmark]: Starting MPV process")
  (with-current-buffer (get-buffer-create "*mpv*")
    (ansi-color-for-comint-mode-on)
    (comint-mode))
  (set-process-filter (start-process-shell-command
                       "bookmark-mpv" "*mpv*"
                       (concat "mpv " +bookmark/mpv-args " \"" url "\""))
                      #'comint-output-filter))

(defconst +bookmark/dispatch-list
  '((("^https://\\(www.\\)?youtu\\(.\\)?be"
      "\\.mp4$")
     . +bookmark/open-mpv)
    (otherwise . eww))
  "List of pairs of type (PATTERNS . FUNC) which is used in
+BOOKMARK/OPEN-BOOKMARK to handle opening urls.

PATTERNS is a list of one or more regexp patterns (as strings)
which should match particular URLs.  It can also be 'OTHERWISE
which represents the default case if no previous patterns match
the URL.

FUNC is a callable or function which takes one parameter (the URL
as a string) and produces some action of opening it.")

(defun +bookmark/open-bookmark ()
  "Open a bookmark.  Uses +BOOKMARK/DISPATCH-LIST to dispatch
opening the url to some handler function."
  (interactive)
  (let* ((bookmarks (+bookmark/bookmarks))
         (choice (completing-read "Choose bookmark: " (mapcar #'car bookmarks) nil t))
         (pair (assoc choice bookmarks #'string=)))
    (if (null pair)
        (error (format "`%s' is not a valid bookmark" choice))
      (message "[bookmark]: Opening `%s` (`%s`)" (car pair) (cdr pair))
      (let* ((url (cdr pair))
             (dispatch-choice
              (cl-loop
               for (patterns . func) in +bookmark/dispatch-list
               if (or (eq patterns 'otherwise)
                     (cl-some
                      #'(lambda (pattern) (string-match pattern url))
                      patterns))
               return func)))
        (message "dispatch-choice=%S" dispatch-choice)
        (funcall dispatch-choice (cdr pair))))))

(provide 'org-bookmark)
;;; bookmark.el ends here
