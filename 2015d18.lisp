;;;2015 day 18

(in-package :aoc-2015)

(defday 18
  :test-input
  ".#.#.#
...##.
#....#
..#...
#.#..#
####.."
  :parse ((make-array (list (position #\newline input)
                            (1+ (count #\newline input)))
                      :initial-contents (s:lines input)))
  :p1 ()
  :p2 ())
