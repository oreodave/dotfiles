(defvar input (uiop:read-file-string "puzzle-2-input.txt"))
;; Each newline represents a new round, which we should parse on the go

(defun sensible-convert-input (str)
  (cond
    ((or (string= str "X") (string= str "A")) 0)
    ((or (string= str "Y") (string= str "B")) 1)
    ((or (string= str "Z") (string= str "C")) 2)))

;; Round 1
(defvar rounds
  (with-input-from-string (stream input)
    (loop
      for strategy = (read-line stream nil)
      until (null strategy)
      collect
      (let ((opponent (subseq strategy 0 1))
            (yours (subseq strategy 2 3)))
        (list (sensible-convert-input opponent) (sensible-convert-input yours))))))

(loop
  for round in rounds
  until (null round)
  sum
  (destructuring-bind (opp you) round
    (+
     1 you ;; base score
     (cond  ; outcome score
       ((eq you opp) 3)
       ((eq (mod (+ 1 opp) 3) you) 6)
       (t 0)))))

;; Round 2.

;; We can still use the same rounds data as previously, just
;; reinterpret it in when doing the sum.

(defun get-correct-choice (opponent outcome)
  (case outcome
    (0 (mod (- opponent 1) 3))
    (1 opp)
    (2 (mod (+ 1 opponent) 3))
    (t 0)))

(loop for round in rounds
      sum
      (destructuring-bind (opp you) round
        (let ((choice (get-correct-choice opp you)))
          (+ 1 choice
             (case you ;; outcome -> score
               (0 0)
               (1 3)
               (2 6)
               (t 0))))))
