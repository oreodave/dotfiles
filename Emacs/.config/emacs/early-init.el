;;; early-init.el --- My custom early-init.el
;; Author: Aryadev Chavali <aryadev@aryadevchavali.com
;; This file is NOT part of GNU Emacs.
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
(push '(alpha . 90) default-frame-alist)
(advice-add #'x-apply-session-resources :override #'ignore)
