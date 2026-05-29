;;;2015 day 9

(in-package :aoc-2015)

(defday 9
  :test-input '("London to Dublin = 464"
                "London to Belfast = 518"
                "Dublin to Belfast = 141")
  :parse ((let ((map (make-hash-table :test 'equal)))
            (dolist (line input)
              (let ((parts (delete-if (lambda (w) (or (string= w "to")
                                                      (string= w "=")))
                                      (str:words line))))
                (setf (third parts) (parse-integer (third parts)))
                (push (subseq parts 1 3) (gethash (first parts) map))))
            map))
  :p1 ((loop :for k :being :each :hash-key :of input
             :minimize (bfs-distance k input)))
  :p2 ())

(defun bfs-distance (start graph)
  (loop :with visited := (list start)
        :and queue := (list start)
        :and distance := 0
        :until (null queue)
        :for current := (pop queue)
        :do (push current visited)
            (loop :for (v dist) :in (gethash current graph)
                  :unless (find v visited :test #'string=)
                    :do ;;(push v visited)
                        (a:appendf queue (list v))
                        (incf distance dist))
        :finally (return distance)))
