#lang htdp/isl+

(require "../util/provide.rkt")

(provide INIT-HC INIT-CH INIT-CC)

(require 2htdp/abstraction)
(require "../connect-four.rkt")
(require "../gui.rkt")

;; ----------------------------------------------------------------------------

;; An automated connect-four player that only looks one move ahead

(define MOVES-AHEAD 1)

(define 0..W (range 0 W 1))

;; A State is a
;;   (make-state [Maybe Side] [List-of Natural] [Maybe [List-of ChoiceResult]])
(define-struct state [winner moves nexts])

;; INIT-STATE : State
(define INIT-STATE (make-state #false (range 0 W 1) #false))

;; A ChoiceResult is a (make-choice-result Natural State)
(define-struct choice-result [move state])

;; choice-result-winner : ChoiceResult -> [Maybe Side]
(define (choice-result-winner c)
  (state-winner (choice-result-state c)))

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

;; tests for next-state/1
(define next-state/1 (next-state/n 1))

(check-expect (next-state/1
               INIT-STATE
               X
               (list (list X X #false #false #false #false)
                     (list O O O #false #false #false)
                     (list X X X #false #false #false)
                     (list O O O #false #false #false)
                     (list X #false #false #false #false #false)
                     (list O O O #false #false #false)
                     (list X X X #false #false #false)))
              (make-state
               X
               (list 2 6)
               (list (make-choice-result 2 (make-state X '() #false))
                     (make-choice-result 6 (make-state X '() #false)))))
(check-expect (next-state/1
               INIT-STATE
               X
               (list (list X X #false #false #false #false)
                     (list O O O #false #false #false)
                     (list X X #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list O X X #false #false #false)))
              (make-state
               #false
               (list 1)
               (list (make-choice-result 1 NO-IMM-WIN-1))))
(check-expect (next-state/1
               INIT-STATE
               X
               (list (list X X #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list X O X #false #false #false)
                     (list O O #false #false #false #false)
                     (list X #false #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list O X X #false #false #false)))
              (make-state
               #false
               (list 4)
               (list (make-choice-result 4 NO-IMM-WIN-1))))
(check-expect (next-state/1
               INIT-STATE
               X
               (list (list O X X #false #false #false)
                     (list O X O #false #false #false)
                     (list X O X #false #false #false)
                     (list O O #false #false #false #false)
                     (list X X #false #false #false #false)
                     (list O O #false #false #false #false)
                     (list O X X #false #false #false)))
              (make-state
               #false
               (list 0)
               (list (make-choice-result 0 NO-IMM-WIN-1))))

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
              (only-paths
               #false
               (list (make-choice-result
                      1
                      (paths
                       #false
                       (list (make-choice-result
                              4
                              (only-paths
                               #false
                               (list (make-choice-result 5 NO-IMM-WIN-1))))
                             (make-choice-result
                              5
                              (only-paths
                               #false
                               (list (make-choice-result 4 NO-IMM-WIN-1)))))
                       NO-IMM-WIN-2))
                     (make-choice-result
                      4
                      (paths
                       #false
                       (list (make-choice-result
                              0
                              (only-paths
                               #false
                               (list (make-choice-result 1 NO-IMM-WIN-1))))
                             (make-choice-result
                              1
                              (only-paths
                               #false
                               (list (make-choice-result 0 NO-IMM-WIN-1)))))
                       NO-IMM-WIN-2)))))

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
              (only-paths
               #false
               (list
                (make-choice-result
                 0
                 (paths
                  #false
                  (list
                   (make-choice-result
                    1
                    (only-paths
                     #false
                     (list (make-choice-result 4 NO-IMM-WIN-1))))
                   (make-choice-result
                    4
                    (only-paths
                     #false
                     (list (make-choice-result 1 NO-IMM-WIN-1)))))
                  NO-IMM-WIN-2))
                (make-choice-result 1 NO-IMM-WIN-3)
                (make-choice-result
                 4
                 (paths
                  #false
                  (list
                   (make-choice-result
                    0
                    (only-paths
                     #false
                     (list (make-choice-result 1 NO-IMM-WIN-1))))
                   (make-choice-result
                    1
                    (only-paths
                     #false
                     (list (make-choice-result 0 NO-IMM-WIN-1)))))
                  NO-IMM-WIN-2)))))

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
              (only-paths
               X
               (list
                (make-choice-result
                 1
                 (paths
                  X
                  (list
                   (make-choice-result
                    0
                    (only-paths
                     X
                     (list (make-choice-result 4 (make-state X '() #false)))))
                   (make-choice-result
                    4
                    (only-paths
                     X
                     (list (make-choice-result 0 (make-state X '() #false))))))
                  (only-paths
                   X
                   (list
                    (make-choice-result 0 (make-state X '() #false))
                    (make-choice-result 4 (make-state X '() #false))))))
                (make-choice-result
                 4
                 (paths
                  X
                  (list
                   (make-choice-result
                    1
                    (only-paths
                     X
                     (list (make-choice-result 5 (make-state X '() #false)))))
                   (make-choice-result
                    5
                    (only-paths
                     X
                     (list (make-choice-result 1 (make-state X '() #false))))))
                  (only-paths
                   X
                   (list
                    (make-choice-result 1 (make-state X '() #false))
                    (make-choice-result 5 (make-state X '() #false)))))))))
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
              (only-paths
               X
               (list
                (make-choice-result
                 1
                 (paths
                  X
                  (list
                   (make-choice-result
                    0
                    (only-paths
                     X
                     (list (make-choice-result 4 (make-state X '() #false)))))
                   (make-choice-result
                    4
                    (only-paths
                     X
                     (list (make-choice-result 0 (make-state X '() #false))))))
                  (only-paths
                   X
                   (list
                    (make-choice-result 0 (make-state X '() #false))
                    (make-choice-result 4 (make-state X '() #false)))))))))

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
              (only-paths
               #false
               (list
                (make-choice-result
                 1
                 (paths
                  #false
                  (list
                   (make-choice-result
                    4
                    (only-paths
                     #false
                     (list (make-choice-result 5 NO-IMM-WIN-1))))
                   (make-choice-result
                    5
                    (only-paths
                     #false
                     (list (make-choice-result 4 NO-IMM-WIN-1)))))
                  NO-IMM-WIN-2))
                (make-choice-result
                 4
                 (paths
                  #false
                  (list
                   (make-choice-result
                    0
                    (only-paths
                     #false
                     (list (make-choice-result 1 NO-IMM-WIN-1))))
                   (make-choice-result
                    1
                    (only-paths
                     #false
                     (list (make-choice-result 0 NO-IMM-WIN-1)))))
                  NO-IMM-WIN-2))
                (make-choice-result
                 6
                 (only-paths
                  #false
                  (list
                   (make-choice-result
                    6
                    (paths
                     #false
                     (list
                      (make-choice-result
                       6
                       (paths
                        #false
                        (list
                         (make-choice-result
                          6
                          (make-state #false (list 0 1 2 3 4 5) #false)))
                        NO-IMM-WIN-0)))
                     NO-IMM-WIN-1))))))))

;; ----------------------------------------------------------------------------

;; best-outcomes : Side Board Natural [List-of Natural] -> State
(define (best-outcomes s b n mvs)
  (local [(define s* (other-side s))]
    (cond
      [(winning-board? s b) (make-state s '() #false)]
      [(winning-board? s* b) (make-state s* '() #false)]
      [(zero? n) (make-state #false mvs #false)]
      [(empty? mvs) (make-state #false '() '())]
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
               (define winning-choices
                 (filter winning-choice? next-outcomes))]
         (cond
           [(not (empty? winning-choices))
            (make-state s
                        (map choice-result-move winning-choices)
                        winning-choices)]
           [else
            (local [;; losing-choice? : ChoiceResult -> Boolean
                    (define (losing-choice? entry)
                      (equal? (choice-result-winner entry) s*))
                    (define non-losing-choices
                      (filter (compose not losing-choice?)
                              next-outcomes))]
              (cond
                [(not (empty? non-losing-choices))
                 (make-state #false
                             (map choice-result-move non-losing-choices)
                             non-losing-choices)]
                [else
                 (make-state s*
                             (map choice-result-move next-outcomes)
                             next-outcomes)]))]))])))

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

;; ----------------------------------------------------------------------------

;; States for no immediate winner

;; only-paths : [Maybe Side] [List-of Choice-Result] -> State
(define (only-paths winner choices)
  (make-state winner
              (map choice-result-move choices)
              choices))

;; paths : [Maybe Side] [List-of Choice-Result] State -> State
(define (paths winner meaningful-choices default)
  (make-state winner
              0..W
              (choice-paths meaningful-choices default)))

;; choice-paths :
;; [List-of ChoiceResult] State -> [List-of ChoiceResult]
(define (choice-paths meaningful-choices default)
  (choice-paths/a meaningful-choices 0 default))

;; choice-paths/a :
;; [List-of ChoiceResult] Natural State -> [List-of ChoiceResult]
(define (choice-paths/a meaningful-choices start default)
  (cond [(= W start) '()]
        [(empty? meaningful-choices)
         (cons
          (make-choice-result start default)
          (choice-paths/a '() (add1 start) default))]
        [(= start (choice-result-move (first meaningful-choices)))
         (cons
          (first meaningful-choices)
          (choice-paths/a (rest meaningful-choices) (add1 start) default))]
        [else
         (cons
          (make-choice-result start default)
          (choice-paths/a meaningful-choices (add1 start) default))]))

(define NO-IMM-WIN-0 (make-state #false 0..W #false))

(define NO-IMM-WIN-1
  (paths #false '() NO-IMM-WIN-0))

(define NO-IMM-WIN-2
  (paths #false '() NO-IMM-WIN-1))

(define NO-IMM-WIN-3
  (paths #false '() NO-IMM-WIN-2))

(define NO-IMM-WIN-4
  (paths #false '() NO-IMM-WIN-3))

(check-expect NO-IMM-WIN-1
              (make-state
               #false
               0..W
               (build-list W
                           (λ (i)
                             (make-choice-result i NO-IMM-WIN-0)))))

(check-expect NO-IMM-WIN-2
              (make-state
               #false
               0..W
               (build-list W
                           (λ (i)
                             (make-choice-result i NO-IMM-WIN-1)))))

(check-expect NO-IMM-WIN-3
              (make-state
               #false
               0..W
               (build-list W
                           (λ (i)
                             (make-choice-result i NO-IMM-WIN-2)))))

(check-expect NO-IMM-WIN-4
              (make-state
               #false
               0..W
               (build-list W
                           (λ (i)
                             (make-choice-result i NO-IMM-WIN-3)))))

