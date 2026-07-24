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
;;  Sets up elpaca, use-package and no-littering then loads the literate
;; system to get my actual configuration.
;;; Code:

;;; Setup elpaca
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))

(require 'elpaca-menu-org)
(setopt elpaca-lock-file (concat elpaca-directory "versions.el"))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Setup benchmark to get current statistics - enable only if profiling.
;; (elpaca benchmark-init
;;   (require 'benchmark-init)
;;   (add-hook 'elpaca-after-init-hook 'benchmark-init/deactivate)
;;   (benchmark-init/activate))

(setq use-package-enable-imenu-support t
      use-package-always-demand nil
      use-package-always-defer nil
      use-package-hook-name-suffix nil
      use-package-compute-statistics t)

(elpaca use-package
  (require 'use-package))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(use-package no-littering
  :ensure t
  :demand t
  :init
  (setq no-littering-etc-directory (expand-file-name ".config/" user-emacs-directory)
        no-littering-var-directory (expand-file-name ".var/" user-emacs-directory))
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load-file custom-file))

(use-package org
  :ensure t)

(use-package literate
  :after (no-littering org)
  :load-path "elisp/"
  :init
  :config
  ;; Preload some modules early if we're running a daemon.
  (thread-last
    (lambda ()
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
    (add-hook 'elpaca-after-init-hook)
    (when (daemonp)))

  (+literate/load-config)
  (add-hook 'kill-emacs-hook #'+literate/compile-config)

  (setq gc-cons-threshold (* 100 1024 1024) ; ~100MiB
        gc-cons-percentage 0.1 ; 10% of heap allocation => collect garbage
        read-process-output-max (* 5 1024 1024) ; ~5MiB
        ))

(use-package gnutls
  :demand t
  :config
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem"))

(provide 'init)
;;; init.el ends here
