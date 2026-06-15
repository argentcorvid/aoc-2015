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
             :do (vformat "~&*new start point ~a" k)
             :minimize (bfs-distance k input)))
  :p2 ())

(defun bfs-distance (start graph) ;;this is doing directed graph, need undirected!
  (loop :with visited := (list)
        :and queue := (list start)
        :and distance := 0
        :until (null queue)
        :for current := (pop queue)
        :do (vformat "~&visiting ~a" current)
            (push current visited)
            (loop :for (dest dist) :in (gethash current graph)
                  :unless (find dest visited :test #'string=)
                    :do (vformat "~& destination ~a not yet visited, adding to queue" dest)
                       (a:appendf queue (list dest))
                  :do (vformat "~& distance to destination ~a: ~d, adding to total" dest dist)
                      (incf distance dist)
                      (vformat "~& total now: ~d" distance))
        :finally (return distance)))
