;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)
; General
(package! elcord)
(package! wttrin)
(package! base16-theme :recipe (:host github :repo "belak/base16-emacs"))
; Coding
(package! counsel-etags)
(package! py-yapf)
; Arduino
(package! arduino-mode)
(package! company-arduino)
