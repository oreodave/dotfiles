;;; private/gentemplate/config.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar +gentemplate/profile-url
  "https://github.com/oreodave/"
  "Profile to download templates from on github.")

(defvar +gentemplate/template-list
  (list "CTemplate" "CPPTemplate" "PythonTemplate" "NodeTemplate" "ArduinoTemplate" "JavaTemplate")
  "List of templates to use, relative to the profile-url")

(defun +gentemplate/offline ()
  (eq (cl-list-length (network-interface-list)) 1))

(defun +gentemplate/copy-template (template-name dest)
  "Copy a template project via it's `template-name' to a folder called `dest'"
  (copy-directory (expand-file-name (concat "~/Code/Templates/" template-name)) dest))

(after! (ivy magit-clone)
  (defun +gentemplate/download-template (template-name dest)
    "Download a given template via its `template-name' to the `dest' folder"
    (magit-clone-regular (concat +gentemplate/profile-url template-name) dest nil))

  (defun +gentemplate/generate-template ()
    (interactive)
    (ivy-read
     "Enter template: "
     +gentemplate/template-list
     :action
     (lambda (template-name)
       (let ((dir (read-directory-name "Enter directory to download to: "))
             (offline (+gentemplate/offline)))
         (if offline
             (+gentemplate/copy-template template-name dir)
           (+gentemplate/download-template template-name dir)))))))