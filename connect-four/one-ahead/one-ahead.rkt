#lang htdp/isl+

(require "../util/provide.rkt")

(provide INIT-HC INIT-CH INIT-CC)

(require "../connect-four.rkt")
(require "../gui.rkt")

;; ----------------------------------------------------------------------------

;; An automated connect-four player that only looks one move ahead

(define MOVES-AHEAD 1)

;; next-moves/n : Natural -> [Side Board -> [List-of Natural]]
;; Goes 2*n levels deep.
(define (next-moves/n n)
  (local [;; next-moves : Side Board -> [List-of Natural]
          (define (next-moves s b)
            (filter-moves s b (* 2 n) (valid-moves b)))]
    next-moves))

;; next-moves : Side Board -> [List-of Natural]
;; Goes 2 levels deep: one turn for s, and one turn for the other side
(define next-moves (next-moves/n MOVES-AHEAD))

;; tests for next-moves
(check-expect (next-moves
               X
               (list (list X X #false #false #false #false)
                     (list O O O #false #false #false)
                     (list X X X #false #false #false)
                     (list O O O #false #false #false)
                     (list X #false #false #false #false #false)
                     (list O O O #false #false #false)
                     (list X X X #false #false #false)))
              (list 2 6))
(check-expect (next-moves
               X
               (list (list X X #false #false #false #false)
                     (list O O O #false #false #false)
                     (list X X #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list O X X #false #false #false)))
              (list 1))
(check-expect (next-moves
               X
               (list (list X X #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list X O X #false #false #false)
                     (list O O #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list O X X #false #false #false)))
              (list 4))
(check-expect (next-moves
               X
               (list (list O X X #false #false #false)
                     (list O X O #false #false #false)
                     (list X O X #false #false #false)
                     (list O O #false #false #false #false)
                     (list X X #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list O X X #false #false #false)))
              (list 0))

;; tests for next-moves/n
(define next-moves/2 (next-moves/n 2))

(check-expect (next-moves/2
               X
               (list (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list O X #false #false #false #false)
                     (list O X #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list X #false #false #false #false #false)))
              (list 1 4))
(check-expect (next-moves/2
               X
               (list (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list O X #false #false #false #false)
                     (list O X #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list #false #false #false #false #false #false)))
              (list 0 1 4))

(check-expect (next-moves/2
               X
               (list (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list O O #false #false #false #false)))
              (list 1 4))
(check-expect (next-moves/2
               X
               (list (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list #false #false #false #false #false #false)))
              (list 1))

(check-expect (next-moves/2
               X
               (list (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list O #false #false #false #false #false)
                     (list O #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list #false #false #false #false #false #false)
                     (list X X #false #false #false #false)))
              (list 1 4 6))

;; filter-moves : Side Board Natural [List-of Natural] -> [List-of Natural]
;; Goes n levels deep
(define (filter-moves s b n mvs)
  (result-moves (best-outcomes s b n mvs)))

;; ----------------------------------------------------------------------------

;; A Result is a (make-result [Maybe Side] [List-of Natural])
(define-struct result [winner moves])

;; A ChoiceResult is a (make-choice-result Natural [Maybe Side])
(define-struct choice-result [move winner])

;; best-outcomes : Side Board Natural [List-of Natural] -> Result
(define (best-outcomes s b n mvs)
  (local [(define s* (other-side s))]
    (cond
      [(winning-board? s b) (make-result s mvs)]
      [(winning-board? s* b) (make-result s* mvs)]
      [(zero? n) (make-result #false mvs)]
      [(empty? mvs) (make-result #false '())]
      [else
       (local [;; next-outcome : Natural -> ChoiceResult
               (define (next-outcome c)
                 (local [(define b* (board-play-at b c s))]
                   (make-choice-result
                    c
                    (result-winner
                     (best-outcomes s* b* (sub1 n) (valid-moves b*))))))
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
            (make-result s winning-moves)]
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
                 (make-result #false non-losing-moves)]
                [else
                 (make-result s* mvs)]))]))])))

;; ----------------------------------------------------------------------------

;; The COMPUTER PlayerType and INIT states

(define COMPUTER (make-computer next-moves))

(define HUMAN-v-COMPUTER (make-player-types HUMAN COMPUTER))
(define COMPUTER-v-HUMAN (make-player-types COMPUTER HUMAN))
(define COMPUTER-v-COMPUTER (make-player-types COMPUTER COMPUTER))

(define INIT-HC (make-game X EMPTY-BOARD HUMAN-v-COMPUTER))
(define INIT-CH (make-game X EMPTY-BOARD COMPUTER-v-HUMAN))
(define INIT-CC (make-game X EMPTY-BOARD COMPUTER-v-COMPUTER))

