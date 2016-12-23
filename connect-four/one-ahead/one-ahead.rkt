#lang htdp/isl+

(require "../util/provide.rkt")

(provide INIT-HC INIT-CH INIT-CC)

(require "../connect-four.rkt")
(require "../gui.rkt")

;; ----------------------------------------------------------------------------

;; An automated connect-four player that only looks one move ahead

(define MOVES-AHEAD 1)

;; A State is a (make-state [Maybe Side] [List-of Natural])
(define-struct state [winner moves])

;; INIT-STATE : State
(define INIT-STATE (make-state #false (range 0 W 1)))

;; next-state/n : Natural -> [Side Board -> State]
;; Goes 2*n levels deep.
(define (next-state/n n)
  (local [;; next-state : State Side Board -> State
          (define (next-state state s b)
            (best-outcomes s b (* 2 n) (valid-moves b)))]
    next-state))

;; next-state : State Side Board -> State
;; Goes 2 levels deep: one turn for s, and one turn for the other side
(define next-state (next-state/n MOVES-AHEAD))

;; tests for next-state
(check-expect (next-state
               INIT-STATE
               X
               (list (list X X #false #false #false #false)
                     (list O O O #false #false #false)
                     (list X X X #false #false #false)
                     (list O O O #false #false #false)
                     (list X #false #false #false #false #false)
                     (list O O O #false #false #false)
                     (list X X X #false #false #false)))
              (make-state X (list 2 6)))
(check-expect (next-state
               INIT-STATE
               X
               (list (list X X #false #false #false #false)
                     (list O O O #false #false #false)
                     (list X X #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list O X X #false #false #false)))
              (make-state #false (list 1)))
(check-expect (next-state
               INIT-STATE
               X
               (list (list X X #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list X O X #false #false #false)
                     (list O O #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list O X X #false #false #false)))
              (make-state #false (list 4)))
(check-expect (next-state
               INIT-STATE
               X
               (list (list O X X #false #false #false)
                     (list O X O #false #false #false)
                     (list X O X #false #false #false)
                     (list O O #false #false #false #false)
                     (list X X #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list O X X #false #false #false)))
              (make-state #false (list 0)))

;; tests for next-state/n
(define next-state/2 (next-state/n 2))

(check-expect (next-state/2
               INIT-STATE
               X
               (list (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list O X #false #false #false #false)
                     (list O X #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list X #false #false #false #false #false)))
              (make-state #false (list 1 4)))
(check-expect (next-state/2
               INIT-STATE
               X
               (list (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list O X #false #false #false #false)
                     (list O X #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list #false #false #false #false #false #false)))
              (make-state #false (list 0 1 4)))

(check-expect (next-state/2
               INIT-STATE
               X
               (list (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list O O #false #false #false #false)))
              (make-state X (list 1 4)))
(check-expect (next-state/2
               INIT-STATE
               X
               (list (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list #false #false #false #false #false #false)))
              (make-state X (list 1)))

(check-expect (next-state/2
               INIT-STATE
               X
               (list (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list O #false #false #false #false #false)
                     (list O #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list X X #false #false #false #false)))
              (make-state #false (list 1 4 6)))

;; ----------------------------------------------------------------------------

;; A ChoiceResult is a (make-choice-result Natural State)
(define-struct choice-result [move state])

;; choice-result-winner : ChoiceResult -> [Maybe Side]
(define (choice-result-winner c)
  (state-winner (choice-result-state c)))

;; best-outcomes : Side Board Natural [List-of Natural] -> State
(define (best-outcomes s b n mvs)
  (local [(define s* (other-side s))]
    (cond
      [(winning-board? s b) (make-state s mvs)]
      [(winning-board? s* b) (make-state s* mvs)]
      [(zero? n) (make-state #false mvs)]
      [(empty? mvs) (make-state #false '())]
      [else
       (local [;; next-outcome : Natural -> ChoiceResult
               (define (next-outcome c)
                 (local [(define b* (board-play-at b c s))]
                   (make-choice-result
                    c
                    (best-outcomes s* b* (sub1 n) (valid-moves b*)))))
               (define next-outcomes
                 (map next-outcome mvs))
               ;; winning-choice? : ChoiceResult -> Boolean
               (define (winning-choice? entry)
                 (equal? (choice-result-winner entry) s))
               (define winning-moves
                 (map choice-result-move
                      (filter winning-choice? next-outcomes)))]
         (cond
           [(not (empty? winning-moves))
            (make-state s winning-moves)]
           [else
            (local [;; losing-choice? : ChoiceResult -> Boolean
                    (define (losing-choice? entry)
                      (equal? (choice-result-winner entry) s*))
                    (define non-losing-moves
                      (map choice-result-move
                           (filter (compose not losing-choice?)
                                   next-outcomes)))]
              (cond
                [(not (empty? non-losing-moves))
                 (make-state #false non-losing-moves)]
                [else
                 (make-state s* mvs)]))]))])))

;; ----------------------------------------------------------------------------

;; The COMPUTER PlayerType and INIT states

(define COMPUTER (make-computer INIT-STATE
                                next-state
                                state-moves))

(define HUMAN-v-COMPUTER (make-player-types HUMAN COMPUTER))
(define COMPUTER-v-HUMAN (make-player-types COMPUTER HUMAN))
(define COMPUTER-v-COMPUTER (make-player-types COMPUTER COMPUTER))

(define INIT-HC (make-game X EMPTY-BOARD HUMAN-v-COMPUTER))
(define INIT-CH (make-game X EMPTY-BOARD COMPUTER-v-HUMAN))
(define INIT-CC (make-game X EMPTY-BOARD COMPUTER-v-COMPUTER))

