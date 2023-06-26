;; Example input: a-b,c-d which denotes [a,b] and [c,d]

;; We want to find if [c,d] < [a,b] or vice versa (complete inclusion)
;; and since we're working with integers, it's simply checking if the
;; bounds are included i.e. c in [a,b] and d in [a,b]

(defvar input (uiop:read-file-string "2022/4-input"))

(defun parse-bound (str)
  "Given STR=\"a-b\" return (a b)"
  (let* ((sep (search "-" str))
         (first (subseq str 0 sep))
         (second (subseq str (+ sep 1))))
    (list (parse-integer first) (parse-integer second))))

(defvar completed-parse
  (with-input-from-string (s input)
    (loop for line = (read-line s nil)
          until (null line)
          collect
          ;; given a-b,c-d we want ((a b) (c d))
          (let* ((sep (search "," line))
                 (first-bound (subseq line 0 sep))
                 (second-bound (subseq line (+ sep 1))))
            (list (parse-bound first-bound) (parse-bound second-bound))))))

(defun complete-inclusion (first-bound second-bound)
  (destructuring-bind (a b) first-bound
    (destructuring-bind (c d) second-bound
      (or
       (and
        (>= a c) (<= a d)
        (>= b c) (<= b d))
       (and
        (>= c a) (<= c b)
        (>= d a) (<= d b))))))

(defvar round-1-answer (length (remove-if #'null
                                          (mapcar (lambda (pair)
                                               (destructuring-bind (first second) pair
                                                 (complete-inclusion first second)))
                                             completed-parse))))

;; Round 2: any overlap at all.  Basically just overhaul the inclusion
;; function and then do the same answer checking.
(defun any-inclusion (first second)
  (destructuring-bind (a b) first
    (destructuring-bind (c d) second
      ;; How about doing this through negation?  [a,b] does not overlap with [c,d] at all if either b < c or a > d.
      (not
       (or
        (< b c)
        (> a d))))))

(defvar round-2-answer (length (remove-if #'null
                                          (mapcar (lambda (pair)
                                               (destructuring-bind (first second) pair
                                                 (any-inclusion first second)))
                                             completed-parse))))
