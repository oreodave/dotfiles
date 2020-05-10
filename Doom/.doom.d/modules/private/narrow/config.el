;;; private/narrow/config.el -*- lexical-binding: t; -*-

(defvar +narrow/narrow-state 't "To narrow or not to narrow. Flips between t and nil")

(defun +narrow/toggle-narrow-state ()
  "Toggle the state of +narrow/narrow-state between 't and 'nil"
  (if (= +narrow/narrow-state 't)
      (setq +narrow/narrow-state nil)
    (setq +narrow/narrow-state 't)))

(defun +narrow/toggle-narrow ()
  (interactive)
  (cond ((+narrow/narrow-state) (narrow-to-defun) (+narrow/toggle-narrow-state))
        (t (widen))))
