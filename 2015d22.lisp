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
            (let ((effect (getf spell :effect)));;somewhwere
              (incf new-hp (getf effect :hp 0))
              (incf new-mp (getf effect :mp 0))
              (decf new-enemy-hp (getf effect :atk 0))
              (a:maxf new-def (getf effect :def 0))
              (when (> (getf effect :dur) 1) ;; in 
                (let* ((new-spell (copy-list spell)))
                  (setf (getf new-spell :effect) (copy-list (getf new-spell :effect)))
                  (decf (getf (getf new-spell :effect) :dur))
                  (push new-spell new-effects))))) ;; here
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
           new-effects
           new-def)
      (setf (values new-effects
                    (d22-entity-hp new-player)
                    (d22-entity-mp new-player)
                    (d22-entity-hp new-enemy)
                    new-def)
            (do-effects state))
      (dolist (spell *d22spells*)
        (let ((spell-cost (getf spell :cost)))
          (unless (or (< player-mp spell-cost)
                      (find spell new-effects :key (lambda (s) (getf s :name))))
            (let ((new-state (make-d22-game-state :player (copy-structure new-player)
                                                  :enemy  (copy-structure new-enemy)
                                                  :total-cost (+ total-cost spell-cost)
                                                  :effects new-effects
                                                  :path (cons spell path)))
                  (inst (getf spell :inst))
                  (effect (getf spell :effect)))
              (let-match (((d22-game-state :player (d22-entity :hp (place new-player-hp) :mp (place new-player-mp))
                                           :enemy (d22-entity :hp (place new-enemy-hp) :atk (place new-enemy-atk))
                                           :total-cost (place new-total-cost)
                                           :best-cost (place new-best-cost)
                                           :path (place new-path)
                                           :effects (place new-effects))
                           new-state))
                (incf new-player-hp (getf inst :hp 0))
                (decf new-player-mp spell-cost)
                (decf new-enemy-hp (getf inst :atk 0))
                (when effect
                  (push spell new-effects))
                (setf (values new-effects
                              new-player-hp
                              new-player-mp
                              new-enemy-hp
                              new-def)
                      (do-effects new-state))
                ;(enemy-turn)
                (when (plusp new-enemy-hp)
                  (decf new-player-hp (max 1 (- new-enemy-atk new-def))))
                (when (plusp new-player-hp)
                  (push new-state states-out)))))))
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
