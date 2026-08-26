;;;2015 day 16

(in-package :aoc-2015)

(defparameter *day16key*
  (apply #'s:dict
         (mapcar (lambda (itm)
                   (if (some #'digit-char-p itm)
                       (parse-integer itm)
                       itm))
                 (s:words
                  "children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1"))))

(defun mutual-intersection (&rest lists)
  (let (res)
    (a:map-combinations (lambda (comb)
                          (push (intersection (first comb) (second comb)) res))
                        lists :length 2)
    (a:when-let (out (reduce #'intersection res))
      (first out))))

(defday 16
  :parse ((let ((sue-table (s:dict)))
            (ppcre:do-register-groups
                ((#'parse-integer sue-number)
                 k1 (#'parse-integer v1)
                 k2 (#'parse-integer v2)
                 k3 (#'parse-integer v3))
                ("(?m)Sue (\\d{1,}): (\\w{1,}): (\\d{1,}), (\\w{1,}): (\\d{1,}), (\\w{1,}): (\\d{1,})"
                 input
                 sue-table)
              
              (s:addhash v1 sue-number (a:ensure-gethash k1 sue-table (s:dict)))
              (s:addhash v2 sue-number (a:ensure-gethash k2 sue-table (s:dict)))
              (s:addhash v3 sue-number (a:ensure-gethash k3 sue-table (s:dict))))))

  :p1 ((labels ((d16-p1-lookup (name)
                  (s:href input name (gethash name *day16key*))))
         (a:map-combinations (lambda (to-lookup)
                               (a:when-let (out (apply #'mutual-intersection (mapcar #'d16-p1-lookup to-lookup)))
                                 (return-from day-16-p1 out)))
                             (a:hash-table-keys *day16key*)
                             :length 3)))
  
  :p2 ((labels ((d16-p2-lookup (name)
                  (s:string-case name
                    (("cats" "trees")
                     (loop :for qty
                           :from (1+ (gethash name *day16key*))
                           :upto (reduce #'max (a:hash-table-keys (gethash name input)))
                           :append (gethash qty (gethash name input))))
                    (("pomeranians" "goldfish")
                     (loop :for qty
                           :from (1- (gethash name *day16key*))
                           :downto (reduce #'min (a:hash-table-keys (gethash name input)))
                           :append (gethash qty (gethash name input))))
                    (t (s:href input name (gethash name *day16key*))))))
         (a:map-combinations (lambda (to-lookup)
                               (a:when-let (out (apply #'mutual-intersection
                                                       (mapcar #'d16-p2-lookup to-lookup)))
                                 (return-from day-16-p2 out)))
                             (a:hash-table-keys *day16key*)
                             :length 3))))
