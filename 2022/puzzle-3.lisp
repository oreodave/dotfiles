(defvar input (uiop:read-file-string "2022/3-input"))

(defun split-string-in-two (s)
  (let ((len (length s)))
    (list (subseq s 0 (/ len 2)) (subseq s (/ len 2)))))

(defvar inputs (with-input-from-string (s input)
                 (loop
                   for line = (read-line s nil)
                   until (null line)
                   collect (split-string-in-two line))))

(defun string-to-clist (str)
  (loop for char across str collect char))

(defun common-types (s1 s2)
  (car (intersection
        (string-to-clist s1)
        (string-to-clist s2))))

(defvar shared (mapcar (lambda (x)
                    (destructuring-bind (s1 s2) x
                      (common-types s1 s2)))
                  inputs))

(defun priority-map (c)
  (if (upper-case-p c)
      (+ 27 (- (char-code c) (char-code #\A)))
      (+ 1 (- (char-code c) (char-code #\a)))))

(defvar round-1-answer (reduce #'+ (mapcar #'priority-map shared)))

;; Round 2

;; Simple recursive algorithm which produces consecutive groups of 3 elements
(defun group-by-3 (lst)
  (if (null lst)
      nil
      (cons
       (list (car lst) (car (cdr lst)) (car (cdr (cdr lst))))
       (group-by-3 (cdr (cdr (cdr lst)))))))

;; Note the use of group-by-3 here
(defvar inputs (group-by-3
                (with-input-from-string (s input)
                  (loop
                    for line = (read-line s nil)
                    until (null line)
                    collect line))))

;; Extend intersection to three
(defun common-types-3 (s1 s2 s3)
  (car
   (intersection
    (string-to-clist s1)
    (intersection
     (string-to-clist s2)
     (string-to-clist s3)))))

;; Extend the destructuring bind and use of common-types-3
(defvar shared (mapcar (lambda (x)
                    (destructuring-bind (s1 s2 s3) x
                      (common-types-3 s1 s2 s3)))
                  inputs))

;; Same as before
(defvar round-2-answer (reduce #'+ (mapcar #'priority-map shared)))
