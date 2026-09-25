;;; iterate drivers and clauses

(in-package :aoc-2015)

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
                                length (combination-length `(length ,sequence))
                                ;result-type (type 'list)
                                )
  "Each unique combination of sequence, ala Alexandria's map-combinations"
  (a:with-unique-names (combs comb-accum)
    (let ((kind (if iter:generate 'generate 'for)))
      `(progn
         (with ,combs = (let (,comb-accum)
                                (A:map-combinations
                                 (lambda (comb)
                                   (push comb ,comb-accum))
                                 ,sequence
                                 :start ,seq-start
                                 :end ,seq-end
                                 :length ,combination-length
                                 :copy t)
                                (nreverse ,comb-accum)))
         (,kind ,combination next
                (if (endp ,combs)
                    (terminate)
                    (pop ,combs)
                   ; (map ,type (a:curry #'elt ,sequence) (pop ,index-combs))
                 ))))))

(defmacro-driver (iter:for item in-heap! heap)
  "Destructively pop successive \"best\" items off of given Serapeum heap"
  `(,(if iter:generate 'generate 'for) ,item next (handler-bind
                                                      ((error (lambda (c) (declare (ignore c)) (terminate))))
                                                    (s:heap-extract-maximum ,heap))))

;; (defmacro-driver (iter:for start-state in-a-star-search-from start-state by-neighbors-func neighbors-func &optional end-pred (end-func #'endp) with-cost-func (cost-func #'identity))
;;   (a:with-unique-names (priority-queue seen current-state)
;;     `(progn
;;        (with ,priority-queue = (s:make-heap :test #'<= :key ,cost-func))
;;        (with ,seen = (s:dict 'equalp))
;;        (when (first-iteration-p)
;;          (s:heap-insert ,priority-queue ,start-state))
;;        (,(if ,generate generate for) ,current-state in-heap! ,priority-queue)
;;        (when (funcall ,end-func ,current-state)
;;          (finish))
;;        (setf (gethash ,current-state ,seen) t)
;;        (collect ,current-state)
;;        )))

(defmacro-clause (collect-to-hashset value &optional into hs-name test (hash-test #'equal))
  "use value as key to store t in a hash table"
  `(accumulate ,value by (lambda (new table)
                           (setf (gethash new table) t)
                           table)
               initial-value (make-hash-table :test ,hash-test)
               into ,hs-name))

(defun a-star-iter (start-state &key neighbors-func (end-state-pred #'endp) (cost-func #'identity))
  (iter outer
                                        ; (with pqueue = (s:make-heap :test #'<= :key cost-func))
    (when (first-iteration-p)
      (s:heap-insert pqueue start-state))
    (while (s:heap-maximum pqueue))
    (for current in-heap! pqueue)
    (when (funcall end-state-pred current)
      (leave current))
    (collect-to-hashset current into seen test #'equalp)
    (iter
      (for n in-sequence (funcall neighbors-func current))
      (unless (or (gethash current seen)
                  (find n (s::heap-vector pqueue) :test #'equalp))
        (in outer (collect-on-heap n into pqueue :test #'<= :key cost-func))))
    (finally (error "Priority queue exhausted without finding end state"))))
