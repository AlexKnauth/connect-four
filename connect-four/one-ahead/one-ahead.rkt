#lang htdp/isl+

(require "../connect-four.rkt")

;; ----------------------------------------------------------------------------

;; An automated connect-four player that only looks one move ahead

;; next-moves : Side Board -> [List-of Natural]

(define (next-moves s b)
  (filter-moves s b (valid-moves b)))

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


;; filter-moves : Side Board [List-of Natural] -> [List-of Natural]
(define (filter-moves s b mvs)
  (cond
    [(empty? mvs) '()]
    [else
     (local [;; winning-move? : Natural -> Boolean
             (define (winning-move? c)
               (winning-board? s (board-play-at b c s)))
             (define winning-moves
               (filter winning-move? mvs))]
       (cond
         [(not (empty? winning-moves)) winning-moves]
         [else
          (local [;; losing-move? : Natural -> Boolean
                  (define (losing-move? c)
                    (local [(define s* (other-side s))
                            (define b* (board-play-at b c s))
                            ;; other-wins? : Natural -> Boolean
                            (define (other-wins? c*)
                              (winning-board? s* (board-play-at b* c* s*)))]
                      (ormap other-wins? (valid-moves b*))))
                  (define non-losing-moves
                    (filter (compose not losing-move?) mvs))]
            (cond
              [(not (empty? non-losing-moves)) non-losing-moves]
              [else mvs]))]))]))

;; ----------------------------------------------------------------------------

;; The COMPUTER PlayerType and INIT states

(define COMPUTER (make-computer next-moves))

(define HUMAN-v-COMPUTER (make-player-types HUMAN COMPUTER))
(define COMPUTER-v-HUMAN (make-player-types COMPUTER HUMAN))
(define COMPUTER-v-COMPUTER (make-player-types COMPUTER COMPUTER))

(define INIT-HC (make-game X EMPTY-BOARD HUMAN-v-COMPUTER))
(define INIT-CH (make-game X EMPTY-BOARD COMPUTER-v-HUMAN))
(define INIT-CC (make-game X EMPTY-BOARD COMPUTER-v-COMPUTER))

