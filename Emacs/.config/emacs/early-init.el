;;; early-init.el --- The first file Emacs loads.   -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Aryadev Chavali

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
;; Sets up some variables and graphical configuration to make Emacs less janky
;; looking while loading.  Shamelessly copies some optimisations from Doom
;; Emacs.
;;; Code:

(setq-default auto-mode-case-fold nil
              bidi-display-reordering 'left-to-right
              bidi-inhibit-bpa t
              bidi-paragraph-direction 'left-to-right
              cursor-in-non-selected-windows nil
              fast-but-imprecise-scrolling t
              frame-inhibit-implied-resize t
              frame-resize-pixelwise t
              gc-cons-percentage 1
              gc-cons-threshold most-positive-fixnum
              highlight-nonselected-windows nil
              idle-update-delay 1.0
              load-prefer-newer noninteractive
              load-prefer-newer noninteractive
              native-comp-always-compile nil
              native-comp-async-jobs-number 4
              native-comp-async-report-warnings-errors 'silent
              native-comp-eln-load-path (list (concat user-emacs-directory ".var/native-compile"))
              package-enable-at-startup nil
              redisplay-skip-fontification-on-input t)

;; don't use x resources lol
(advice-add #'x-apply-session-resources :override #'ignore)

;; turn off the menu bar, tool bar, scroll bar, fringes
;; also set the transparency (active inactive)
(setq-default
 default-frame-alist '((menu-bar-lines       . nil)
                       (tool-bar-lines       . nil)
                       (scroll-bar-lines     . nil)
                       (vertical-scroll-bars . nil)
                       (left-fringe          . 0)
                       (right-fringe         . 0)
                       (alpha                . (80 70)))
 menu-bar-mode nil
 tool-bar-mode nil
 scroll-bar-mode nil)

;; no flash bang, please
(set-face-background 'default "#0a0a0a")
(set-face-foreground 'default "#b6b6b6")

;; Disable making the tool bar
(advice-add #'tool-bar-setup :override #'ignore)

;; Even though we disable the startup screen in the config, we need to do this
;; to ensure it actually doesn't display it
(advice-add #'display-startup-screen :override #'ignore)
