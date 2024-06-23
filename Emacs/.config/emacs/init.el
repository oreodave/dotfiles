;;; init.el --- My custom init.el that starts my Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Aryadev Chavali

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
;; Sets up straight, use package and the literate system.
;;; Code:

;; Before doing anything else, make gc-cons-threshold ridiculously
;; high.  This makes it so we have as few pauses during init as
;; possible.
;; Straight
(setq straight-disable-native-compile nil
      straight-use-package-by-default nil
      straight-check-for-modifications 'live)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
           user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-enable-imenu-support t
      use-package-always-demand nil
      use-package-always-defer nil
      use-package-hook-name-suffix nil
      use-package-compute-statistics t)

(straight-use-package 'use-package)
(straight-use-package 'org)
(straight-use-package 'no-littering)

(setq no-littering-etc-directory (expand-file-name ".config/"  user-emacs-directory)
      no-littering-var-directory (expand-file-name ".local/" user-emacs-directory)
      custom-file (no-littering-expand-etc-file-name "custom.el"))

(load-file custom-file)

;;; Load literate
(load-file (concat user-emacs-directory "elisp/literate.el"))

;; Compile on Emacs quit
(add-hook
 'kill-emacs-hook
 #'+literate/compile-config)

(+literate/load-config)

(when (daemonp)
  (require 'general)
  (require 'evil)
  (require 'ivy)
  (require 'counsel)
  (require 'notmuch)
  (require 'company)
  (require 'org)
  (require 'eglot))

(setq gc-cons-threshold (* 100 1024 1024) ; ~100MiB
      read-process-output-max (* 5 1024 1024) ; ~5MiB
      ;; FIXME: Problem with memory-report after running Emacs for a
      ;; bit, causes a Lisp nesting error, so I just set it up really
      ;; high so it doesn't reach that.
      max-lisp-eval-depth 999999)

(provide 'init)
;;; init.el ends here
