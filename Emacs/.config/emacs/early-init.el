;;; early-init.el --- The first file Emacs loads.   -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Aryadev Chavali

;; Author: Aryadev Chavali <aryadev@aryadevchavali.com>

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
;; Sets up some variables and graphical configuration to make Emacs less janky
;; looking while loading.  Shamelessly copies some optimisations from Doom
;; Emacs.
;;; Code:

(setq-default auto-mode-case-fold nil
              bidi-display-reordering 'left-to-right
              bidi-inhibit-bpa t
              bidi-paragraph-direction 'left-to-right
              cursor-in-non-selected-windows nil)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      gc-cons-percentage 1
      gc-cons-threshold most-positive-fixnum
      highlight-nonselected-windows nil
      idle-update-delay 1.0
      load-prefer-newer t
      native-comp-always-compile nil
      native-comp-async-jobs-number 4
      native-comp-async-report-warnings-errors 'silent
      native-comp-jit-compilation t
      package-enable-at-startup nil
      redisplay-skip-fontification-on-input t)

(startup-redirect-eln-cache ".var/native-compile")

;; Restore GC after startup.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024) ; ~100MiB
                  gc-cons-percentage 0.1 ; 10% of heap allocation => collect garbage
                  read-process-output-max (* 5 1024 1024) ; ~5MiB
                  )))

;; don't use x resources lol
(advice-add #'x-apply-session-resources :override #'ignore)

;; turn off the menu bar, tool bar, scroll bar, also set the transparency
;; (active inactive)
(setq-default
 default-frame-alist '((menu-bar-lines         . 0)
                       (tool-bar-lines         . 0)
                       (vertical-scroll-bars   . nil)
                       (horizontal-scroll-bars . nil)
                       (alpha                  . (87 76))))

;; We load modus vivendi to ensure no light theme on startup - this is reloaded
;; again as part of [[file:config.org::*Modus Themes]]
(load-theme 'modus-vivendi t)

;; Disable making the tool bar
(advice-add #'tool-bar-setup :override #'ignore)

;; Even though we disable the startup screen in the config, we need to do this
;; to ensure it actually doesn't display it
(advice-add #'display-startup-screen :override #'ignore)
