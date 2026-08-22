;;;2015 day 14

(in-package :aoc-2015)

(defday 14
  :test-input
  (s:lines "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")
  :parse ((let (data)
            (dolist (l (s:lines input))
              (push (mapcar (lambda (itm)
                              (if (find-if #'digit-char-p itm)
                                  (parse-integer itm)
                                  itm))
                            (pick-elements (s:words l) 0 3 6 13))
                    data))
            data))
  :p1 ()
  :p2 ())
