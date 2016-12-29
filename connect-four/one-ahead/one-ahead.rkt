#lang htdp/isl+

(require "../util/provide.rkt")

(provide INIT-HC INIT-CH INIT-CC)

(require "../connect-four.rkt")
(require "../gui.rkt")

;; ----------------------------------------------------------------------------

;; An automated connect-four player that only looks one move ahead

(define MOVES-AHEAD 1)

(define 0..W (range 0 W 1))

;; A State is one of:
;;  - #false
;;  - Result

;; A Result is a
;;   (make-state [Maybe Side] [List-of ChoiceResult])
(define-struct result [winner nexts])

;; INIT-STATE : State
(define INIT-STATE #false)

;; state-moves : State -> [List-of Natural]
(define (state-moves s)
  (cond [(false? s) 0..W]
        [else (map choice-result-move (result-nexts s))]))

;; A ChoiceResult is a (make-choice-result Natural State)
(define-struct choice-result [move state])

;; choice-result-winner : ChoiceResult -> [Maybe Side]
(define (choice-result-winner c)
  (result-winner (choice-result-state c)))

;; make-next-state/n : Natural -> [Side Board -> State]
(define (make-next-state/n n)
  (local [;; next-state : State Side Board -> State
          (define (next-state state s b)
            (next-state/depth state s b (* 2 n)))]
    next-state))

;; next-state/depth : Natural -> [Side Board -> State]
;; Goes d levels deep.
(define (next-state/depth state s b d)
  (cond
    [(or (false? state) (zero? d))
     (best-outcomes s b d (valid-moves b))]
    [(result? state)
     (local [;; update-choice-result : ChoiceResult -> ChoiceResult
             (define (update-choice-result c)
               (local [(define mv (choice-result-move c))
                       (define s* (other-side s))
                       (define b* (board-play-at b mv s))]
                 (make-choice-result
                  mv
                  (next-state/depth (choice-result-state c) s* b* (sub1 d)))))
             (define old-next-states
               (map choice-result-state (result-nexts state)))
             (define choices
               (map update-choice-result (result-nexts state)))]
       ;; optimization: if the updated choices result in the same winner
       ;;               possibilities, just use them all without filtering
       (cond
         [(and (andmap result? old-next-states)
               (equal? (map result-winner old-next-states)
                       (map result-winner (map choice-result-state choices))))
          (make-result (result-winner state)
                       choices)]
         [else
          (best-choices s choices)]))]))

;; next-state : State Side Board -> State
;; Goes 2 levels deep: one turn for s, and one turn for the other side
(define next-state (make-next-state/n MOVES-AHEAD))

;; tests for next-state/1
(define next-state/1 (make-next-state/n 1))

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
              (only-paths
               X
               (list (make-choice-result 2 X-IMM-WIN)
                     (make-choice-result 6 X-IMM-WIN))))
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
              (only-paths
               #false
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
              (only-paths
               #false
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
              (only-paths
               #false
               (list (make-choice-result 0 NO-IMM-WIN-1))))

;; tests for next-state/n
(define next-state/2 (make-next-state/n 2))

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
                     (list (make-choice-result 4 X-IMM-WIN))))
                   (make-choice-result
                    4
                    (only-paths
                     X
                     (list (make-choice-result 0 X-IMM-WIN)))))
                  (only-paths
                   X
                   (list
                    (make-choice-result 0 X-IMM-WIN)
                    (make-choice-result 4 X-IMM-WIN)))))
                (make-choice-result
                 4
                 (paths
                  X
                  (list
                   (make-choice-result
                    1
                    (only-paths
                     X
                     (list (make-choice-result 5 X-IMM-WIN))))
                   (make-choice-result
                    5
                    (only-paths
                     X
                     (list (make-choice-result 1 X-IMM-WIN)))))
                  (only-paths
                   X
                   (list
                    (make-choice-result 1 X-IMM-WIN)
                    (make-choice-result 5 X-IMM-WIN))))))))
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
                     (list (make-choice-result 4 X-IMM-WIN))))
                   (make-choice-result
                    4
                    (only-paths
                     X
                     (list (make-choice-result 0 X-IMM-WIN)))))
                  (only-paths
                   X
                   (list
                    (make-choice-result 0 X-IMM-WIN)
                    (make-choice-result 4 X-IMM-WIN))))))))

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
                          (only-paths
                           #false
                           (list (make-choice-result 0 #false)
                                 (make-choice-result 1 #false)
                                 (make-choice-result 2 #false)
                                 (make-choice-result 3 #false)
                                 (make-choice-result 4 #false)
                                 (make-choice-result 5 #false)))))
                        NO-IMM-WIN-0)))
                     NO-IMM-WIN-1))))))))

;; ----------------------------------------------------------------------------

;; best-outcomes : Side Board Natural [List-of Natural] -> State
(define (best-outcomes s b n mvs)
  (local [(define s* (other-side s))]
    (cond
      [(winning-board? s b) (imm-win s)]
      [(winning-board? s* b) (imm-win s*)]
      [(zero? n) (make-result #false
                              (map (λ (mv) (make-choice-result mv #false))
                                   mvs))]
      [(empty? mvs) IMM-TIE]
      [else
       (local [;; next-outcome : Natural -> ChoiceResult
               (define (next-outcome c)
                 (local [(define b* (board-play-at b c s))]
                   (make-choice-result
                    c
                    (best-outcomes s* b* (sub1 n) (valid-moves b*)))))]
         (best-choices s (map next-outcome mvs)))])))

;; best-choices : Side [List-of ChoiceResult] -> Result
(define (best-choices s choices)
  (local [(define s* (other-side s))
          ;; winning-choice? : ChoiceResult -> Boolean
          (define (winning-choice? entry)
            (equal? (choice-result-winner entry) s))
          (define winning-choices
            (filter winning-choice? choices))]
    (cond
      [(not (empty? winning-choices))
       (make-result s winning-choices)]
      [else
       (local [;; losing-choice? : ChoiceResult -> Boolean
               (define (losing-choice? entry)
                 (equal? (choice-result-winner entry) s*))
               (define non-losing-choices
                 (filter (compose not losing-choice?)
                         choices))]
         (cond
           [(not (empty? non-losing-choices))
            (make-result #false non-losing-choices)]
           [else
            (make-result s* choices)]))])))

;; ----------------------------------------------------------------------------

;; The state-add-move method

;; state-add-move : State Side Natural -> State
(define (state-add-move state s mv)
  (cond
    [(false? state) #false]
    [(result? state)
     (lookup-move-result (result-nexts state) s mv)]))

;; lookup-move-result : [List-of ChoiceResult] Side Natural -> State
(define (lookup-move-result choices s mv)
  (cond
    [(empty? choices) #false]
    [(cons? choices)
     (if (equal? mv (choice-result-move (first choices)))
         (choice-result-state (first choices))
         (lookup-move-result (rest choices) s mv))]))

;; ----------------------------------------------------------------------------

;; The COMPUTER PlayerType and INIT states

(define COMPUTER (make-computer INIT-STATE
                                next-state
                                state-moves
                                state-add-move))

(define HUMAN-v-COMPUTER (make-player-types HUMAN COMPUTER))
(define COMPUTER-v-HUMAN (make-player-types COMPUTER HUMAN))
(define COMPUTER-v-COMPUTER (make-player-types COMPUTER COMPUTER))

(define INIT-HC (make-game X EMPTY-BOARD HUMAN-v-COMPUTER))
(define INIT-CH (make-game X EMPTY-BOARD COMPUTER-v-HUMAN))
(define INIT-CC (make-game X EMPTY-BOARD COMPUTER-v-COMPUTER))

(define COMPUTER/1 (make-computer INIT-STATE
                                  next-state/1
                                  state-moves
                                  state-add-move))
(define COMPUTER/2 (make-computer INIT-STATE
                                  next-state/2
                                  state-moves
                                  state-add-move))

(define INIT-CC-UNBALANCED
  (make-game X EMPTY-BOARD (make-player-types COMPUTER/1 COMPUTER/2)))

;; ----------------------------------------------------------------------------

;; States for no immediate winner

;; imm-win : [Maybe Side] -> Result
(define (imm-win winner) (make-result winner '()))

(define X-IMM-WIN (imm-win X))
(define O-IMM-WIN (imm-win O))
(define IMM-TIE (imm-win #false))

;; only-paths : [Maybe Side] [List-of Choice-Result] -> State
(define (only-paths winner choices)
  (make-result winner choices))

;; paths : [Maybe Side] [List-of Choice-Result] State -> State
(define (paths winner meaningful-choices default)
  (make-result winner (choice-paths meaningful-choices default)))

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

(define NO-IMM-WIN-0
  (paths #false '() #false))

(define NO-IMM-WIN-1
  (paths #false '() NO-IMM-WIN-0))

(define NO-IMM-WIN-2
  (paths #false '() NO-IMM-WIN-1))

(define NO-IMM-WIN-3
  (paths #false '() NO-IMM-WIN-2))

(define NO-IMM-WIN-4
  (paths #false '() NO-IMM-WIN-3))

(check-expect NO-IMM-WIN-1
              (make-result
               #false
               (build-list W
                           (λ (i)
                             (make-choice-result i NO-IMM-WIN-0)))))

(check-expect NO-IMM-WIN-2
              (make-result
               #false
               (build-list W
                           (λ (i)
                             (make-choice-result i NO-IMM-WIN-1)))))

(check-expect NO-IMM-WIN-3
              (make-result
               #false
               (build-list W
                           (λ (i)
                             (make-choice-result i NO-IMM-WIN-2)))))

(check-expect NO-IMM-WIN-4
              (make-result
               #false
               (build-list W
                           (λ (i)
                             (make-choice-result i NO-IMM-WIN-3)))))

