;;;2015 day 24

(in-package :aoc-2015)

(defun quantum-entanglement (&rest numbers)
  (apply #'* numbers))

(defun greater-legroom-p (pkg-list-a pkg-list-b
                          &aux
                            (len-a (length pkg-list-a))
                            (len-b (length pkg-list-b)) )
  (or (< len-a len-b)
      (and (= len-a len-b)
           (< (apply #'quantum-entanglement pkg-list-a)
              (apply #'quantum-entanglement pkg-list-b)))))

(defun collect-packages (input &key part-2)
  (loop :with heap := (s:make-heap :test #'< :key #'second) ;only works when stopping after the smallest group, otherwise, use greater-legroom-p. not a problem because that is part of the requirement
        :and n-groups := (if part-2 4 3)
        :with required-group-weight := (/ (apply #'+ input) n-groups) ; will return integer if evenly divisible (part of requirement) if not, a fraction, which wont ever be = to the sum of any combination
        :for n :from 2 :upto (floor (length input) 2)
        :do (a:map-combinations
             (lambda (comb)
               (when (= (apply #'+ comb)
                        required-group-weight)
                 (s:heap-insert heap (list comb (apply #'quantum-entanglement comb)))))
             input :length n)
          :thereis (s:heap-maximum heap)))

(iter:defmacro-clause (collect-on-heap value &optional into heap-name test (heap-test #'>=) key (key-func #'identity))
  "collect into a heap from Serapeum"
  `(accumulate ,value
               by (lambda (new so-far)
                    (s:heap-insert so-far new)
                    so-far)
               initial-value (s:make-heap :test ,heap-test :key ,key-func)
               into ,heap-name))

(iter:defmacro-driver (iter:for combination in-combinations-of sequence
                                &optional
                                start (seq-start 0)
                                end (seq-end `(length ,sequence)) 
                                length (combination-length `(length ,sequence)))
  "Each unique combination of sequence, ala Alexandria's map-combinations"
  (a:with-unique-names (index-combs comb-accum)
    (let ((kind (if iter:generate 'generate 'for)))
      `(progn
         (with ,index-combs = (let (,comb-accum)
                                (A:map-combinations
                                 (lambda (comb)
                                   (push comb ,comb-accum))
                                 (s:range ,seq-start ,seq-end)
                                 :length ,combination-length
                                 :copy t)
                                (nreverse ,comb-accum)))
         (,kind ,combination next
                (if (endp ,index-combs)
                    (terminate)
                    (map 'list (a:curry #'elt ,sequence) (pop ,index-combs))))))))

(defun collect-packages-iter (input &key part-2)
  (iter:iter outer
    (with n-groups = (if part-2 4 3))
    (with required-group-weight = (/ (apply #'+ input) n-groups))
    (for n from 2)
    (iter
      (for comb in-combinations-of input :length n)
      (when (= (apply #'+ comb)
               required-group-weight)
        (in outer
            (collect-on-heap (list comb (apply #'quantum-entanglement comb))
                             into heap
                             :test #'<
                             :key #'second))))
    (thereis (s:heap-maximum heap))))

(defday 24
  :test-input
  (str:lines
   "1
2
3
4
5
7
8
9
10
11")
  :parse ((mapcar #'parse-integer input))
  :p1 ((collect-packages input))
  :p1-test-expected '((11 9) 99)
  :p1-test ((day-24-p1 (day-24-parse %test-input%)))
  :p2 ((collect-packages input :part-2 t))
  :p2-test-expected '((11 4) 44)
  :p2-test ((day-24-p2 (day-24-parse %test-input%))))


(defun day-24-p1run (&optional (input-file *day24input*))
  (day-24-p1 (day-24-parse (uiop:read-file-lines input-file))))

(defun day-24-p2run (&optional (input-file *day24input*))
  (day-24-p2 (day-24-parse (uiop:read-file-lines input-file))))
