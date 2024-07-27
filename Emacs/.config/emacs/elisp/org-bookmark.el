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
(autoload #'mpv-start-process "mpv")

(defvar org-bookmark/file (expand-file-name (concat org-directory "/bookmarks.org")))

(defun org-bookmark/--get-heading-data ()
  "In an org-mode buffer, with point on a heading: get the title,
tags and url."
  (let ((heading-components (org-heading-components))
        (tags (org-get-tags))
        (url (org-entry-get (point-marker) "URL")))
    (list
     (nth 4 heading-components)
     (cl-remove-if #'(lambda (tag) (string= tag "bookmark")) tags)
     url)))

(defun org-bookmark/--get-all-heading-data ()
  "In an org-mode buffer, get all the heading data (titles, tags and
urls)."
  (cl-remove-if
   #'(lambda (x) (member "DONE" (nth 1 x)))
   ;; Extract all headings with :bookmark: tag
   (org-scan-tags
    #'org-bookmark/--get-heading-data
    (cdr (org-make-tags-matcher ":bookmark:"))
    nil)))

(defun org-bookmark/--format-heading-data (data)
  "Format the heading data extracted into a pair of (TITLE+TAG
. URL)."
  (cl-destructuring-bind (name tags url) data
    (cons
     (format "%s %s" name
             (substring-no-properties
              (string-join tags ":")))
     url)))

(defvar org-bookmark/--cache nil
  "Cached alist constructed from bookmarks file of form (TITLE+TAG
. URL).")

(defvar org-bookmark/--cache-last-modified nil
  "Last modified time for bookmarks file as a float.")

(defun org-bookmark/bookmarks ()
  "Get all bookmarks from ORG-BOOKMARK/FILE in alist format.  Results
are cached for faster lookup."
  (with-current-buffer (find-file-noselect org-bookmark/file)
    (let ((cur-last-modified (float-time (visited-file-modtime))))
      (when (or (null org-bookmark/--cache) ; no cache
               (null org-bookmark/--cache-last-modified) ; no last modified
               (not (= cur-last-modified org-bookmark/--cache-last-modified))) ; file has been modified
        (setq org-bookmark/--cache-last-modified cur-last-modified
              org-bookmark/--cache (mapcar
                                 #'org-bookmark/--format-heading-data
                                 (org-bookmark/--get-all-heading-data))))))
  org-bookmark/--cache)

(defconst org-bookmark/dispatch-list
  '((("^https://\\(www.\\)?youtu\\(.\\)?be"
      "\\.mp4$")
     . mpv-start-process)
    (otherwise . eww))
  "List of pairs of type (PATTERNS . FUNC) which is used in
ORG-BOOKMARK/OPEN-BOOKMARK to handle opening urls.

PATTERNS is a list of one or more regexp patterns (as strings)
which should match particular URLs.  It can also be 'OTHERWISE
which represents the default case if no previous patterns match
the URL.

FUNC is a callable or function which takes one parameter (the URL
as a string) and produces some action of opening it.")

(defun org-bookmark/open-bookmark ()
  "Open a bookmark.  Uses ORG-BOOKMARK/DISPATCH-LIST to dispatch
opening the url to some handler function."
  (interactive)
  (let* ((bookmarks (org-bookmark/bookmarks))
         (choice (completing-read "Choose bookmark: " (mapcar #'car bookmarks) nil t))
         (pair (assoc choice bookmarks #'string=)))
    (if (null pair)
        (error (format "`%s' is not a valid bookmark" choice))
      (message "[bookmark]: Opening `%s` (`%s`)" (car pair) (cdr pair))
      (let* ((url (cdr pair))
             (dispatch-choice
              (cl-loop
               for (patterns . func) in org-bookmark/dispatch-list
               if (or (eq patterns 'otherwise)
                     (cl-some
                      #'(lambda (pattern) (string-match pattern url))
                      patterns))
               return func)))
        (message "dispatch-choice=%S" dispatch-choice)
        (funcall dispatch-choice (cdr pair))))))

(provide 'org-bookmark)
;;; bookmark.el ends here
