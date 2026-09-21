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

(defvar *d22hardness* 0)

(defstruct d22-entity
  (hp 0)
  (mp 0)
  (atk 0)
  (def 0))

(defstruct (d22-game-state
            (:copier nil))
  (player (apply #'make-d22-entity *d22player-initial*))
  (enemy (apply #'make-d22-entity *d22boss-initial*))
  (total-cost 0)
  (best-cost most-positive-fixnum)
  (path (list))
 ; (edges (copy-list *d22spells*))
  (effects (list)))

(defun d22-spell-name (spell)
  (getf spell :name))

(defun copy-d22-game-state (s)
  "makes deep copy of game state (instead of default shallow one)"
  (check-type s d22-game-state)
  (let-match (((d22-game-state player enemy total-cost path effects) s))
    (make-d22-game-state :player (copy-structure player)
                         :enemy (copy-structure enemy)
                         :total-cost total-cost
                         :path (copy-tree path)
                         :effects (copy-tree effects))))
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
  (let ((new-state (copy-d22-game-state state)))
    (let-match (((d22-game-state :player (d22-entity :hp (place player-hp) :mp (place player-mp))
                                 :enemy (d22-entity :hp (place enemy-hp))
                                 :effects (place effects))
                 new-state))
      (let* ((new-def 0)
             states-out) 
        (decf player-hp *d22hardness*)
        (when effects ;; do effects before player turn
          (setf (values effects
                        player-hp
                        player-mp
                        enemy-hp
                        new-def)
                (do-effects state)))
        (dolist (spell *d22spells*)
          (let-match (((plist :cost spell-cost
                              :inst spell-inst
                              :effect spell-effect
                              :name spell-name)
                       spell)) 
            (when (and (>= player-mp spell-cost)
                       (not (find spell-name effects :key #'d22-spell-name))) ;for each possible spell
              (let ((spell-state (copy-d22-game-state new-state)))
                (let-match (((d22-game-state :player (d22-entity :hp (place spell-player-hp)
                                                                 :mp (place spell-player-mp))
                                             :enemy (d22-entity :hp (place spell-enemy-hp)
                                                                :atk spell-enemy-atk)
                                             :total-cost (place spell-total-cost)
                                             :path (place spell-path)
                                             :effects (place spell-effects))
                             spell-state))
                                        ; (setf spell-path (s:append1 spell-path spell)) ;;doing equalp to prevent duplicating states in the queue takes a lot more work
                  (incf spell-total-cost spell-cost) ;do player turn
                  (incf spell-player-hp (getf spell-inst :hp 0))
                  (decf spell-player-mp spell-cost)
                  (decf spell-enemy-hp (getf spell-inst :atk 0))
                  (when spell-effect
                    (push (copy-tree spell) spell-effects))
                  (decf spell-player-hp *d22hardness*)
                  (when spell-effects ; do effects before enemy turn
                    (setf (values spell-effects
                                  spell-player-hp
                                  spell-player-mp
                                  spell-enemy-hp
                                  new-def)
                          (do-effects spell-state)))
                  (when (plusp spell-enemy-hp) ;;enemy turn, can't act if dead
                    (decf spell-player-hp (max 1 (- spell-enemy-atk new-def))))
                  (when (plusp spell-player-hp) ;don't add to neighbors if literal dead-end
                    (push spell-state states-out)))))))
        states-out))))

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
  :p1 ((a-star (make-d22-game-state)
               :neighbors-func #'d22-neighbors
               :end-state-pred (a:compose #'not #'plusp #'d22-entity-hp #'d22-game-state-enemy)
               :cost-func #'d22-game-state-total-cost))
  :p2 ((let ((*d22hardness* 1))
         (a-star (make-d22-game-state)
                 :neighbors-func #'d22-neighbors
                 :end-state-pred (a:compose #'not #'plusp #'d22-entity-hp #'d22-game-state-enemy)
                 :cost-func #'d22-game-state-total-cost))))
