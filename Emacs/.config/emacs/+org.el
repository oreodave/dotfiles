(defun dx:org-insert-item (direction)
  "Shamelessly copied from Doom Emacs, better insert item"
  (let* ((context
	  (save-excursion
	    (when (bolp)
	      (back-to-indentation)
	      (forward-char))
	    (org-element-lineage
	     (org-element-context)
	     '(table table-row headline inlinetask item plain-list)
	     t)))
	 (type (org-element-type context)))
    (cond ((memq type '(item plain-list))
	   (let ((marker (org-element-property :bullet context))
		 (pad (save-excursion
			(org-beginning-of-item)
			(back-to-indentation)
			(- (point) (line-beginning-position)))))
	     (save-match-data
	       (pcase direction
		 (`below
		  (org-end-of-item)
		  (backward-char)
		  (end-of-line)
		  (if (and marker (string-match "\\([0-9]+\\)\\([).] *\\)" marker))
		      (let ((l (line-number-at-pos)))
			(org-insert-item)
			(when (= l (line-number-at-pos))
			  (org-next-item)
			  (org-end-of-line)))
		    (insert "\n" (make-string pad 32) (or marker ""))))
		 (`above
		  (org-beginning-of-item)
		  (if (and marker (string-match-p "[0-9]+[).]" marker))
		      (org-insert-item)
		    (insert (make-string pad 32) (or marker ""))
		    (save-excursion (insert "\n")))))))
	   (when (org-element-property :checkbox context)
	     (insert "[ ] ")))

	  ((memq type '(table table-row))
	   (pcase direction
	     ('below (save-excursion (org-table-insert-row t))
		     (org-table-next-row))
	     ('above (save-excursion (org-shiftmetadown))
		     (+org/table-previous-row))))

	  ((let ((level (or (org-current-level) 1)))
	     (pcase direction
	       (`below
		(let (org-insert-heading-respect-content)
		  (goto-char (line-end-position))
		  (org-end-of-subtree)
		  (insert "\n" (make-string level ?*) " ")))
	       (`above
		(org-back-to-heading)
		(insert (make-string level ?*) " ")
		(save-excursion (insert "\n"))))
	     (when-let* ((todo-keyword (org-element-property :todo-keyword context))
			 (todo-type (org-element-property :todo-type context)))
	       (org-todo (cond ((eq todo-type 'done)
				(car (+org-get-todo-keywords-for todo-keyword)))
			       (todo-keyword)
			       ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
	       (not (evil-emacs-state-p)))
      (evil-insert 1))))

(defun dx:org-insert-item-below ()
  (interactive)
  (dx:org-insert-item 'below))

(defun dx:org-insert-item-above ()
  (interactive)
  (dx:org-insert-item 'above))
