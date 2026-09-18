;;;2015 day 22
;;;https://www.reddit.com/r/adventofcode/comments/3xspyl/day_22_solutions/cy927kk/

(in-package :aoc-2015)

(defparameter *d22boss-initial*
  (list :hp 58 :atk 9 )
  "Hit Points: 58
Damage: 9")

(defparameter *d22player-initial*
  (list :hp 50 :mp 500))

(defparameter *d22spells*
  (list (list :name :magic-missile :cost 53 :inst (list :atk 4 :hp 0) :effect nil)
        (list :name :drain :cost 73 :inst (list :atk 2 :hp 2) :effect nil)
        (list :name :shield :cost 113 :effect (list :dur 6 :def 7) :inst nil)
        (list :name :poison :cost 173 :effect (list :dur 6 :atk 3) :inst nil)
        (list :name :recharge :cost 229 :effect (list :dur 5 :mp 101) :inst nil)))

(defstruct d22-entity
  (hp 0)
  (mp 0)
  (atk 0)
  (def 0))

(defstruct d22-game-state
  (player (apply #'make-d22-entity *d22player-initial*))
  (enemy (apply #'make-d22-entity *d22boss-initial*))
  (total-cost 0)
  (best-cost most-positive-fixnum)
  (path (list))
 ; (edges (copy-list *d22spells*))
  (effects (list)))

(defun d22-spell-name (spell)
  (getf spell :name))

(defun do-effects (state)
  (with-accessors ((player d22-game-state-player)
                   (enemy d22-game-state-enemy)
                   (effects d22-game-state-effects))
      state
    (with-accessors ((player-hp d22-entity-hp)
                     (player-mp d22-entity-mp))
        player
      (with-accessors ((enemy-hp d22-entity-hp))
          enemy
        (let ((new-effects (list))
              (new-def 0)
              (new-hp player-hp) 
              (new-mp player-mp)
              (new-enemy-hp enemy-hp)
              (effects-in effects))
          (dolist (spell effects-in)
            (let ((effect (getf spell :effect)))
              (incf new-hp (getf effect :hp 0))
              (incf new-mp (getf effect :mp 0))
              (decf new-enemy-hp (getf effect :atk 0))
              (a:maxf new-def (getf effect :def 0))
              (when (> (getf effect :dur) 1) 
                (let* ((new-spell (copy-list spell)))
                  (setf (getf new-spell :effect) (copy-list (getf new-spell :effect)))
                  (decf (getf (getf new-spell :effect) :dur))
                  (push new-spell new-effects))))) 
          (values new-effects
                  new-hp
                  new-mp
                  new-enemy-hp
                  new-def))))))

(defun d22-neighbors (state)
  (let-match (((d22-game-state :player (d22-entity :hp player-hp :mp player-mp)
                               :enemy (d22-entity :hp enemy-hp :atk enemy-atk)
                               total-cost
                               best-cost
                               path
                               effects)
               state))
    (let* ((new-player (make-d22-entity :hp player-hp :mp player-mp))
           (new-enemy (make-d22-entity :hp enemy-hp :atk enemy-atk))
           states-out
           new-effects ;;
           (new-def 0));;
      (when effects
        (setf (values new-effects
                      (d22-entity-hp new-player)
                      (d22-entity-mp new-player)
                      (d22-entity-hp new-enemy)
                      new-def)
              (do-effects state)))
      (dolist (spell *d22spells*)
        (let ((spell-cost (getf spell :cost)))
          (when (and (>= player-mp spell-cost)
                     (not (find spell new-effects :key (a:rcurry #'getf :name))))
            (let ((spell-player (copy-structure new-player))
                  (spell-enemy  (copy-structure new-enemy))
                  (spell-total-cost (+ total-cost spell-cost))
                  (spell-effects (copy-list new-effects))
                  (spell-path (cons spell path))
                  (spell-inst (getf spell :inst)))
              (incf (d22-entity-hp spell-player) (getf spell-inst :hp 0))
              (decf (d22-entity-mp spell-player) spell-cost)
              (decf (d22-entity-hp spell-enemy) (getf spell-inst :atk 0))
              (a:when-let (spell-effect (getf spell :effect))
                (push spell-effect spell-effects))
              (let ((spell-state (make-d22-game-state :player spell-player
                                                      :enemy spell-enemy
                                                      :total-cost spell-total-cost
                                                      :effects spell-effects
                                                      :path spell-path)))
                (when spell-effects
                  (setf (values (d22-game-state-effects spell-effects)
                                (d22-entity-hp spell-player)
                                (d22-entity-mp spell-player)
                                (d22-entity-hp spell-enemy)
                                new-def)
                        (do-effects spell-state)))
                (when (plusp (d22-entity-hp spell-enemy))
                  (decf (d22-entity-hp spell-player) (max 1 (- (d22-entity-atk spell-enemy) new-def))))
                (when (plusp (d22-entity-hp spell-player))
                  (push spell-state states-out)))))))
      states-out)))

(defun a-star (start-state &key neighbors-func (end-state-pred #'endp) (cost-func #'identity))
  (let ((pqueue (s:make-heap :test #'<= :key cost-func))
        (done (s:dict 'equalp)))
    (s:heap-insert pqueue start-state)
    (handler-case
        (loop
          (let ((current (s:heap-extract-maximum pqueue)))
            (when (funcall end-state-pred current)
              (return-from a-star current))
            (setf (gethash current done) t)
            (s:do-each (neighbor (funcall neighbors-func current))
              (unless (or (gethash neighbor done)
                          (find neighbor (s::heap-vector pqueue) :test #'equalp)
                          )
                (s:heap-insert pqueue neighbor)))))
      (error nil
        (fresh-line)
        (princ "emptied the queue without finding the end state")))))

(defday 22
  :test-input ""
  :parse ()
  :p1 ((a-star (make-d22-game-state) :neighbors-func #'d22-neighbors :end-state-pred (a:compose #'not #'plusp #'d22-entity-hp #'d22-game-state-enemy) :cost-func #'d22-game-state-total-cost))
  :p2 ())
