;;; early-init.el --- My custom early-init.el
;; Author: Aryadev Chavali <aryadev@aryadevchavali.com
;; This file is NOT part of GNU Emacs.
;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      package-enable-at-startup nil
      frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(alpha . 85) default-frame-alist)
(advice-add #'x-apply-session-resources :override #'ignore)
