;;; ada-mode.el --- My custom ada-mode that colourises buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Aryadev Chavali

;; Author: Aryadev Chavali <aryadev@aryadevchavali.com>
;; Keywords: faces, languages

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

;; This provides colourising for ada files.  Is not compatible with
;; standard ada-mode due to name conflicts.  Personally I'd rather
;; have a mode for colourising a buffer which is what this is supposed
;; to be.

;; I feel ada-mode™ (the official one) tries to do too many things at
;; once, so I want something that does the basics.  Also want to take
;; this opportunity to make a new mode and see what facilities Emacs
;; provides for it.

;;; Other notes:
;; TODO: Handle indenting

;;; Code:

(require 'generic-x)

(define-generic-mode ada-mode
  '("--") ; Comments
  '("abort" "else" "new" "return"
    "abs" "elsif" "not" "reverse"
    "abstract" "end" "null"
    "accept" "entry" "select"
    "access" "exception" "of" "separate"
    "aliased" "exit" "or" "subtype"
    "all" "others" "synchronized"
    "and" "for" "out"
    "array" "function" "overriding" "tagged"
    "at" "task"
    "generic" "package" "terminate"
    "begin" "goto" "pragma" "then"
    "body" "private" "type"
    "if" "procedure"
    "case" "in" "protected" "until"
    "constant" "interface" "use"
    "is" "raise"
    "declare" "range" "when"
    "delay" "limited" "record" "while"
    "delta" "loop" "rem" "with"
    "digits" "renames"
    "do" "mod" "requeue" "xor") ; Keywords
  nil
  '("\\.ad\\(b\\|s\\)")
  nil)

(provide 'ada-mode)
;;; ada-mode.el ends here
