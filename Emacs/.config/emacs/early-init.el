;;; early-init.el --- My custom early-init.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Aryadev Chavali

;; Author: Aryadev Chavali <aryadev@aryadevchavali.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Sets up some variables and graphical configuration to make Emacs
;; less janky looking while loading
;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      package-enable-at-startup nil
      frame-inhibit-implied-resize nil
      frame-resize-pixelwise t
      native-comp-async-jobs-number 4
      native-comp-eln-load-path (list (concat user-emacs-directory ".local/native-compile"))
      native-comp-always-compile nil
      native-comp-async-report-warnings-errors 'silent)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(alpha . (85 65)) default-frame-alist)
(advice-add #'x-apply-session-resources :override #'ignore)
