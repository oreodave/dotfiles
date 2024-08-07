;;; mpv.el --- Open MPV through Emacs!               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Aryadev Chavali

;; Author: Aryadev Chavali <aryadev@aryadevchavali.com>
;; Keywords: convenience

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

(autoload #'ffap-guesser "ffap")
(autoload #'ansi-color-for-comint-mode-on "ansi-color")
(autoload #'comint-output-filter "comint")

(defvar mpv-ytdl-args "--ytdl-format=\"bestvideo[height<=1080][fps<=60]+bestaudio/best[height<=1080]\""
  "Arguments for ytdl in mpv format.")

(defvar mpv-args "-v --profile=fast --hwdec=auto-copy"
  "General arguments for mpv binary.")

(defun mpv--make-args ()
  (concat mpv-args " " mpv-ytdl-args))

(defun mpv-start-process (url)
  (message "[mpv]: Starting mpv on `%s'" url)
  (with-current-buffer (get-buffer-create "*mpv*")
    (ansi-color-for-comint-mode-on)
    (comint-mode))
  (set-process-filter (start-process-shell-command
                       "mpv" "*mpv*"
                       (concat "mpv " (mpv--make-args) " \"" url "\""))
                      #'comint-output-filter))

(defvar mpv--history nil)

(defun mpv-open-video ()
  (interactive)
  (let ((url (completing-read "Enter URL: " nil nil t (ffap-guesser) mpv--history)))
    (mpv-start-process url)
    (switch-to-buffer "*mpv*")))

(provide 'mpv)
;;; mpv.el ends here
