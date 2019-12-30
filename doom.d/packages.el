;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)
; General
(package! elcord) ; to flex
(package! wttrin) ; weather in emacs? yes please
(package! wakatime-mode) ; wakatime
(package! darkroom-mode :recipe (:host github :repo "joaotavora/darkroom"))
(package! powerthesaurus)
; Coding
(package! counsel-etags) ; tags are cool
(package! py-yapf) ; formatting
; Arduino
(package! arduino-mode) ; mandatory, though I might make my own module for this
(package! company-arduino) ; intellisense EVERYTHING
