(defvar input (uiop:read-file-string "puzzle-1-input.txt"))
(defvar *sep (format nil "~%~%"))

(defun get-lists (input)
  (let ((pos (search *sep input)))
    (with-input-from-string (s (subseq input 0 pos))
      (let ((converted
              (loop
                for line = (read-line s nil nil)
                while line
                collect (parse-integer line))))
        (if (null pos)
            (list converted)
            (cons converted
                  (get-lists (subseq input (+ pos 2)))))))))

(defvar sums (sort (mapcar (lambda (lst) (reduce #'+ lst)) (get-lists input)) #'>))

;; First challenge
(format t "Top snacks: ~a" (car sums))

;; Second challenge
(let ((first (car sums))
      (second (car (cdr sums)))
      (third (car (cdr (cdr sums)))))
  (format t "~a,~a,~a:>~a" first second third (+ first second third)))
