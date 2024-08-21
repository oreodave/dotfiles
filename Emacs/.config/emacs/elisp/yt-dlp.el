;;; yt-dlp.el --- Using yt-dlp through Emacs         -*- lexical-binding: t; -*-

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

;; Please for the love of god have a yt-dlp config.  We're not dealing with that
;; for you.

;;; Code:

(autoload #'ansi-color-for-comint-mode-on "ansi-color")
(autoload #'comint-mode "comint")
(autoload #'comint-output-filter "comint")

(defun yt-dlp-download-video (&optional url)
  (interactive)
  (let ((url (if (stringp url)
                 url
               (read-string "Enter URL: "))))
    (message "[yt-dlp]: Downloading video `%s'" url)
    (with-current-buffer (get-buffer-create "*yt-dlp*")
      (ansi-color-for-comint-mode-on))
    (set-process-filter (start-process-shell-command
                         "yt-dlp" "*yt-dlp*"
                         (concat "yt-dlp " "\"" url "\""))
                        #'comint-output-filter)
    (switch-to-buffer "*yt-dlp*")))

(provide 'yt-dlp)
;;; yt-dlp.el ends here
