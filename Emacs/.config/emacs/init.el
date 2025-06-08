;;; init.el --- The second file Emacs loads.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024  Aryadev Chavali

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
;;  Sets up straight, use-package and no-littering then loads the literate
;; system to get my actual configuration.
;;; Code:

;;; Setup straight
(setq straight-disable-native-compile nil
      straight-use-package-by-default nil
      straight-vc-git-default-clone-depth 'full
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

;; Setup benchmark to get current statistics - enable only if profiling.
(straight-use-package 'benchmark-init)
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)
(benchmark-init/activate)

(setq use-package-enable-imenu-support t
      use-package-always-demand nil
      use-package-always-defer nil
      use-package-hook-name-suffix nil
      use-package-compute-statistics t)

(straight-use-package 'use-package)
(straight-use-package 'org-mode)
(straight-use-package 'no-littering)

(setq no-littering-etc-directory (expand-file-name ".config/" user-emacs-directory)
      no-littering-var-directory (expand-file-name ".var/" user-emacs-directory)
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
  ;; No need to lazy load this stuff
  (require 'general)
  (require 'evil)
  (require 'dired)
  (require 'consult)
  (require 'notmuch)
  (require 'magit)
  (require 'org)
  (require 'company)
  (require 'eshell)
  (require 'eglot))

(setq gc-cons-threshold (* 100 1024 1024) ; ~100MiB
      gc-cons-percentage 0.1 ; 10% of heap allocation => collect garbage
      read-process-output-max (* 5 1024 1024) ; ~5MiB
      ;; FIXME: Problem with memory-report after running Emacs for a
      ;; bit, causes a Lisp nesting error, so I just set it up really
      ;; high so it doesn't reach that.
      max-lisp-eval-depth 999999
      garbage-collection-messages t)

(provide 'init)
;;; init.el ends here
