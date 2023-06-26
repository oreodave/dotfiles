(defvar input (uiop:read-file-string "2022/5-input"))

;; When we get two newlines, it means the end of the initial state and
;; the start of instructions
(defvar parse-separator (search (format nil "~%~%") input))
(defvar initial-state
  (with-input-from-string (s (subseq input 0 parse-separator))
    (loop
      for line = (read-line s nil)
      until (null line)
      collect line)))

;; the last number, indicating the number of stacks
(defparameter n-stacks (let ((str (car (last initial-state))))
                         (parse-integer (subseq str (- (length str) 1)))))

(defun default-state ()
  (loop for i from 1 to n-stacks
        collect nil))

(defvar state
  (default-state))

#|
conjecture: the nth stack, if it has an entry, has '[' beginning at index 4n;

base case: the 0th stack must begin at index 0 (if at all)

intuition: next stack must start at 0 + 2 (for the stack info) +
1 (for whitespace) + 1 so 4.

inductive hypothesis: for the kth stack [ begins at 4k

proof of induction claim: from 4k we have the following:
4k+1: symbol
4k+2: ]
4k+3: whitespace
4k+4: data for the (k+1 stack)

Immediately 4k+4 = 4(k+1) so by principle of induction we have the
conjecture.  QED.

This gives us all the information we need to make a parser: check
every position and see if it has a [ char.  If so then parse the data
and insert into the index/4th stack!|#

(defun parse-initial-state ()
  (loop
    ;; don't want to parse the last line
    for j in (remove (car (last initial-state)) initial-state)
    do
       (loop
         for i from 0
         for c across j
         do
            (if (char= c #\[)
                (let ((ind (/ i 4))
                      (sym (subseq j (+ i 1) (+ i 2))))
                  (setf (nth ind state) (append (nth ind state) (list sym))))))))


;; Now we have the initial memory layout, we need to parse program code.

;; + 2 because two newlines
(defvar instructions-str (subseq input (+ 2 parse-separator)))

#| Each command is of the following: move ~n from ~a to ~b.

~n is some natural number of crates, ~a is the stack from which we
are taking them and ~b is the stack we are adding them to.  Let's
define this operation first!  |#

(defun move-crates (n a b)
  "Take N number of crates from stack at position A to stack at position B"
  (let ((stack-a (nth a state))
        (stack-b (nth b state)))
    (if (= n 0)
        nil
        (progn
          ;; Pop the first element off the stack
          (setf (nth a state) (cdr stack-a))
          ;; Then cons that onto b
          (setf (nth b state) (cons (car stack-a) stack-b))
          ;; Recur
          (move-crates (- n 1) a b)))))

(defun parse-instruction-str (instruction)
  "Given INSTRUCTION of form \"move n from a to b\", return (n (a - 1) (b - 1))"
  (let ((first (search "move " instruction))
        (second (search "from " instruction))
        (third (search "to " instruction)))
    (list
     (parse-integer (subseq instruction (+ 5 first) (- second 1)))
     ;; Input assumes crates start at 1, but we need it to start at 0
     (- (parse-integer (subseq instruction (+ 5 second) (- third 1))) 1)
     (- (parse-integer (subseq instruction (+ 3 third))) 1))))

(defun perform-instructions (instructions)
  (with-input-from-string (s instructions)
    (loop
      for line = (read-line s nil)
      until (null line)
      collect
      ;; Parse each instruction then move the crates!
      (destructuring-bind (n a b) (parse-instruction-str line)
        (move-crates n a b)))))

(defun first-round ()
  (setq state (default-state))
  (parse-initial-state)
  (perform-instructions instructions-str)
  (let ((ret (mapcar #'car state)))
    (setq state (default-state))
    (reduce (lambda (s1 s2) (concatenate 'string s1 s2)) ret)))

;; Round 2 is pretty simple: the move-crates algorithm is overhauled
;; to keep movements "in-order".  Thankfully I already implemented
;; this by accident when implementing move-crates, so easy!

(defun move-crates-2 (n a b)
  (let ((stack-a (nth a state))
        (stack-b (nth b state)))
    (setf (nth b state)
          (append (loop for i from 1 to n
                        for j in stack-a
                        collect j)
                  stack-b))
    (dotimes (i n)
      (setf stack-a (cdr stack-a)))
    (setf (nth a state) stack-a)))

(defun perform-instructions-2 (instructions)
  (with-input-from-string (s instructions)
    (loop
      for line = (read-line s nil)
      until (null line)
      collect
      ;; Parse each instruction then move the crates!
      (destructuring-bind (n a b) (parse-instruction-str line)
        (move-crates-2 n a b)))))

(defun second-round ()
  (setq state (default-state))
  (parse-initial-state)
  (perform-instructions-2 instructions-str)
  (let ((ret (mapcar #'car state)))
    (setq state (default-state))
    (reduce (lambda (s1 s2) (concatenate 'string s1 s2)) ret)))
