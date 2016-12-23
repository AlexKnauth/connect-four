#lang htdp/isl+

(require "util/provide.rkt")

(provide W H N
         INIT-HH
         game-state?
         game? make-game game-turn game-board game-player-types
         end-state? make-end-state end-state-game end-state-winner
         X O other-side side->string
         make-pos pos-column pos-row
         valid-moves valid-move?
         EMPTY-BOARD board-ref
         board-play-at game-play-at
         winning-board? check-winner
         make-player-types player-types-p1 player-types-p2
         HUMAN
         computer? make-computer
         game-player-type
         continue-game
         )

(require 2htdp/abstraction)
(require "util/list.rkt")

;; ----------------------------------------------------------------------------

(define W 7)
(define H 6)
(define N 4)

;; ----------------------------------------------------------------------------

;; Data Definitions

;; A GameState is one of:
;;  - Game
;;  - EndState
;; game-state? : Any -> Boolean
(define (game-state? v)
  (or (game? v) (end-state? v)))

;; A Game is a (make-game Side Board PlayerTypes)
(define-struct game (turn board player-types))

;; An EndState is a (make-end-state Game [Maybe Side])
(define-struct end-state (game winner))

;; A Board is a [List-of BoardColumn]
;; A BoardColumn is a [List-of Space]

;; A Space is a [Maybe Side]

;; A Side is one of:
;;  - X
;;  - O
(define X 'X)
(define O 'O)

(define-struct pos (column row))
;; A Pos is a (make-pos Natural Natural)
;; where (0 <= column < W) and (0 <= row < H)

;; 5 | | | | | | | |
;; 4 | | | | | | | |
;; 3 | | | | | | | |
;; 2 | | | | | | | |
;; 1 | | | | | | | |
;; 0 | | | | | | | |
;;   ---------------
;;    0 1 2 3 4 5 6

;; valid-column+row? : Integer Integer -> Boolean
(define (valid-column+row? column row)
  (and (<= 0 column) (< column W)
       (<= 0 row)    (< row H)))

;; ----------------------------------------------------------------------------

;; Types of players

;; A PlayerTypes is a (make-player-types PlayerType PlayerType)
(define-struct player-types (p1 p2))

;; A PlayerType is one of:
;;  - HUMAN
;;  - Computer
(define HUMAN 'HUMAN)

;; A Computer is a
;;   (make-computer State
;;                  [State Side Board -> State]
;;                  [State -> [List-of Natural]])
;; where State is a type associated with each instance
(define-struct computer (state next-state state-moves))

;; get-computer-next-state : Computer Side Board -> State
;; where State is the type associated with the Computer c
(define (get-computer-next-state c s b)
  ((computer-next-state c) (computer-state c) s b))

;; get-computer-state-moves : Computer State -> [List-of Natural]
;; where State is the type associated with the Computer c
(define (get-computer-state-moves c state)
  ((computer-state-moves c) state))

;; update-computer-state : Computer State -> Computer
(define (update-computer-state c state)
  (make-computer state (computer-next-state c) (computer-state-moves c)))

;; get-player-type : PlayerTypes Side -> PlayerType
(define (get-player-type ts s)
  (cond [(equal? s X) (player-types-p1 ts)]
        [(equal? s O) (player-types-p2 ts)]))

;; update-player-type : PlayerTypes Side PlayerType -> PlayerTypes
(define (update-player-type ts s t)
  (cond [(equal? s X) (make-player-types t (player-types-p2 ts))]
        [(equal? s O) (make-player-types (player-types-p1 ts) t)]))

;; game-player-type : Game -> PlayerType
(define (game-player-type g)
  (get-player-type (game-player-types g) (game-turn g)))

;; update-game-player-type : Game PlayerType -> Game
(define (update-game-player-type g t)
  (make-game (game-turn g)
             (game-board g)
             (update-player-type (game-player-types g) (game-turn g) t)))

;; ----------------------------------------------------------------------------

;; Side functions

;; other-side : Side -> Side
(define (other-side s)
  (cond [(equal? s X) O]
        [(equal? s O) X]))

;; side->string : Side -> String
(define (side->string s)
  (cond [(equal? s X) "Red"]
        [(equal? s O) "Black"]))

;; ----------------------------------------------------------------------------

;; Board and BoardColumn functions

;; board-ref : Board Pos -> Space
(define (board-ref b p)
  (list-ref (list-ref b (pos-column p))
            (pos-row p)))

;; board-columns : Board -> [List-of BoardColumn]
(define board-columns identity)

;; board-rows : Board -> [List-of [List-of Space]]
(define (board-rows b)
  (apply map list b))

;; board-set-column : Board Natural BoardColumn -> Board
(define board-set-column list-set)

;; column-set-space : BoardColumn Natural Space -> BoardColumn
(define column-set-space list-set)

;; board-set : Board Pos Space -> Board
(define (board-set b p s)
  (board-set-column
   b
   (pos-column p)
   (column-set-space (list-ref b (pos-column p))
                     (pos-row p)
                     s)))

;; build-board : [Pos -> Space] -> Board
(define (build-board f)
  (build-list
   W
   (λ (x)
     (build-list H (λ (y) (f (make-pos x y)))))))

(define EMPTY-BOARD
  (build-board (λ (p) #false)))

;; ----------------------------------------------------------------------------

;; INIT states

(define HUMAN-v-HUMAN (make-player-types HUMAN HUMAN))
(define INIT-HH (make-game X EMPTY-BOARD HUMAN-v-HUMAN))

;; ----------------------------------------------------------------------------

;; Valid Moves

;; valid-move? : Board Natural -> Boolean
(define (valid-move? b c)
  (valid-move-in-column? (list-ref b c)))

;; valid-move-in-column? : BoardColumn -> Boolean
(define (valid-move-in-column? column)
  (member? #false column))

;; valid-moves : Board -> [List-of Natural]
(define (valid-moves b)
  (local [;; valid-column? : Natural -> Boolean
          (define (valid-column? c)
            (valid-move? b c))]
    (filter valid-column? (build-list W identity))))

;; ----------------------------------------------------------------------------

;; Playing at a certain place

;; game-play-at : Game Natural -> Game
(define (game-play-at g column)
  (make-game (other-side (game-turn g))
             (board-play-at (game-board g) column (game-turn g))
             (game-player-types g)))

;; board-play-at : Board Natural Side -> Board
(define (board-play-at b column side)
  (board-set-column
   b
   column
   (board-column-play-at (list-ref b column) side)))

;; board-column-play-at : BoardColumn Side -> BoardColumn
(check-expect (board-column-play-at
               (list #false #false #false #false #false #false)
               X)
              (list X #false #false #false #false #false))
(check-expect (board-column-play-at
               (list X #false #false #false #false #false)
               X)
              (list X X #false #false #false #false))
(check-expect (board-column-play-at
               (list X X #false #false #false #false)
               O)
              (list X X O #false #false #false))

(define (board-column-play-at c s)
  (cond [(empty? c) (error "invalid move: out of space")]
        [else
         (if (false? (first c))
             (cons s (rest c))
             (cons (first c) (board-column-play-at (rest c) s)))]))

;; ----------------------------------------------------------------------------

;; Displaying alerts when someone wins

;; check-winner : Game -> GameState
(define (check-winner g)
  (cond [(winning-board? (other-side (game-turn g)) (game-board g))
         (make-end-state g (other-side (game-turn g)))]
        [else g]))

;; ----------------------------------------------------------------------------

;; Continuing the game and resetting it

;; continue-game : Game -> [Maybe GameState]
(define (continue-game g)
  (continue-game/player-type
   (game-player-type g)
   g))

;; continue-game/player-type : PlayerType Game -> [Maybe GameState]
(define (continue-game/player-type t g)
  (cond [(equal? t HUMAN) #false]
        [else
         (local [(define next-state
                   (get-computer-next-state t (game-turn g) (game-board g)))
                 (define next-moves
                   (get-computer-state-moves t next-state))]
           (cond
             [(empty? next-moves)
              (cond [(empty? (valid-moves (game-board g)))
                     (make-end-state g #false)]
                    [else #false])]
             [else
              (check-winner (game-play-at
                             (update-game-player-type
                              g
                              (update-computer-state t next-state))
                             (random-element next-moves)))]))]))

;; ----------------------------------------------------------------------------

;; Determining whether a board is winning

;; winning-board? : Side Board -> Boolean
(define (winning-board? s b)
  (local [;; winning-list? : BoardColumn -> Boolean
          (define (winning-list? c)
            (winning-list-of-spaces? s c 0))]
    (or (ormap winning-list? (board-columns b))
        (ormap winning-list? (board-rows b))
        (ormap winning-list? (board-left-up-diagonals b))
        (ormap winning-list? (board-right-up-diagonals b)))))

;; winning-list-of-spaces? : Side [List-of Space] Natural -> Boolean
;; accumulator i represents the number in a row seen so far
(define (winning-list-of-spaces? s c i)
  (cond [(empty? c) #false]
        [else
         (if (equal? s (first c))
             (or (<= N (add1 i))
                 (winning-list-of-spaces? s (rest c) (add1 i)))
             (winning-list-of-spaces? s (rest c) 0))]))

;; ----------------------------------------------------------------------------

;; Diagonals

;; board-right-up-diagonals : Board -> [List-of [List-of Space]]
(define (board-right-up-diagonals b)
  (for/list ([x (range (- H) W 1)])
    (for/list ([y (in-range 0 H 1)])
      (if (valid-column+row? (+ x y) y)
          (board-ref b (make-pos (+ x y) y))
          #false))))

(check-expect (board-right-up-diagonals
               (list (list X X #false #false #false #false)
                     (list O O O #false #false #false)
                     (list X X X #false #false #false)
                     (list O O O #false #false #false)
                     (list X #false #false #false #false #false)
                     (list O O O #false #false #false)
                     (list X X X #false #false #false)))
              (list (list #false #false #false #false #false #false)
                    (list #false #false #false #false #false #false)
                    (list #false #false #false #false #false #false)
                    (list #false #false #false #false #false #false)
                    (list #false #false #false #false #false #false)
                    (list #false X O #false #false #false)
                    (list X O X #false #false #false)
                    (list O X O #false #false #false)
                    (list X O #false #false #false #false)
                    (list O #false O #false #false #false)
                    (list X O X #false #false #false)
                    (list O X #false #false #false #false)
                    (list X #false #false #false #false #false)))

;; board-left-up-diagonals : Board -> [List-of [List-of Space]]
(define (board-left-up-diagonals b)
  (for/list ([x (in-range 0 (+ W H) 1)])
    (for/list ([y (in-range 0 H 1)])
      (if (valid-column+row? (- x y) y)
          (board-ref b (make-pos (- x y) y))
          #false))))

(check-expect (board-left-up-diagonals
               (list (list X X #false #false #false #false)
                     (list O O O #false #false #false)
                     (list X X X #false #false #false)
                     (list O O O #false #false #false)
                     (list X #false #false #false #false #false)
                     (list O O O #false #false #false)
                     (list X X X #false #false #false)))
              (list (list X #false #false #false #false #false)
                    (list O X #false #false #false #false)
                    (list X O #false #false #false #false)
                    (list O X O #false #false #false)
                    (list X O X #false #false #false)
                    (list O #false O #false #false #false)
                    (list X O #false #false #false #false)
                    (list #false X O #false #false #false)
                    (list #false #false X #false #false #false)
                    (list #false #false #false #false #false #false)
                    (list #false #false #false #false #false #false)
                    (list #false #false #false #false #false #false)
                    (list #false #false #false #false #false #false)))

