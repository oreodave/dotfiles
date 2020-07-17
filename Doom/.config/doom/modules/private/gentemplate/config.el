;;; private/gentemplate/config.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst +gentemplate/repo-url
  "https://github.com/oreodave/"
  "Repository url to download templates")

(defconst +gentemplate/template-list
  (list "CTemplate" "CPPTemplate" "PythonTemplate" "NodeTemplate" "ArduinoTemplate" "JavaTemplate")
  "List of templates to use, relative to the repo-url")

(defun +gentemplate/offline ()
  "Check if user is offline"
  (eq (cl-list-length (network-interface-list)) 1))

(defun +gentemplate/copy-template (template-name dest)
  "Copy a template project via it's `template-name' to a folder called `dest'"
  (copy-directory (expand-file-name (concat "~/Code/Templates/" template-name)) dest))

(after! magit
  (defun +gentemplate/download-template (template-name dest)
    "Download a given template via its `template-name' to the `dest' folder"
    (magit-clone-regular (concat +gentemplate/repo-url template-name) dest nil))

  (defun +gentemplate/generate-template ()
    (interactive)
    (let ((template-name (completing-read
                          "Enter template: "
                          +gentemplate/template-list))
          (dir (read-directory-name "Enter directory to download to: "))
          (offline (+gentemplate/offline)))
      (if offline
          (+gentemplate/copy-template template-name dir)
        (+gentemplate/download-template template-name dir)))))
