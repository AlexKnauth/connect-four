#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/abstraction)
(require "../util/list.rkt")

;; ----------------------------------------------------------------------------

(define W 7)
(define H 6)
(define N 4)

(define BLACK "black")
(define WHITE "white")
(define RED "red")
(define YELLOW "yellow")
(define BROWN "brown")
(define SEMI-TRANSPARENT-GRAY (make-color 127 127 127 127))

(define SQUARE-SIZE 50)
(define CIRCLE-RADIUS 20)

(define FRAME-COLOR YELLOW)
(define SQUARE-OUTLINE-COLOR BROWN)
(define SQUARE (overlay (circle CIRCLE-RADIUS "solid" WHITE)
                        (square SQUARE-SIZE "outline" SQUARE-OUTLINE-COLOR)
                        (square SQUARE-SIZE "solid" FRAME-COLOR)))
(define X-SQUARE (overlay (circle CIRCLE-RADIUS "solid" RED) SQUARE))
(define O-SQUARE (overlay (circle CIRCLE-RADIUS "solid" BLACK) SQUARE))

(define BOARD-WIDTH (* W SQUARE-SIZE))
(define BOARD-HEIGHT (* H SQUARE-SIZE))
(define BOARD-BG (empty-scene BOARD-WIDTH BOARD-HEIGHT))

(define TURN-MSG-WIDTH BOARD-WIDTH)
(define TURN-MSG-HEIGHT 30)
(define TURN-MSG-FONT-SIZE 20)
(define TURN-MSG-FONT-COLOR BLACK)
(define TURN-MSG-BG (empty-scene TURN-MSG-WIDTH TURN-MSG-HEIGHT))

(define WIDTH (max TURN-MSG-WIDTH BOARD-WIDTH))
(define HEIGHT (+ TURN-MSG-HEIGHT BOARD-HEIGHT))

(define SEMI-TRANSPARENT-GRAY-RECTANGLE
  (rectangle WIDTH HEIGHT "solid" SEMI-TRANSPARENT-GRAY))

(define ALERT-BOX-FONT-SIZE 20)
(define ALERT-BOX-FONT-COLOR RED)
(define ALERT-BOX-WIDTH 300)
(define ALERT-BOX-HEIGHT 150)
(define ALERT-BOX-BG (empty-scene ALERT-BOX-WIDTH ALERT-BOX-HEIGHT))

;; ----------------------------------------------------------------------------

;; The main function

;; A WorldState is one of:
;;  - Game
;;  - Alert

;; An Alert is a (make-alert String Game WorldState)
(define-struct alert (msg prev next))

;; main : WorldState -> WorldState
(define (main start)
  (big-bang start
    [on-mouse handle-mouse]
    [on-key handle-key]
    [to-draw draw-world]))

;; ----------------------------------------------------------------------------

(define-struct game (turn board player-types))
;; A Game is a (make-game Side Board PlayerTypes)

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
;;  - (make-computer [Side Board -> [List-of Natural]])
(define HUMAN 'HUMAN)
(define-struct computer (next-moves))

;; get-player-type : PlayerTypes Side -> PlayerType
(define (get-player-type ts s)
  (cond [(equal? s X) (player-types-p1 ts)]
        [(equal? s O) (player-types-p2 ts)]))

;; game-player-type : Game -> PlayerType
(define (game-player-type g)
  (get-player-type (game-player-types g) (game-turn g)))

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

;; check-winner : Game -> WorldState
(define (check-winner g)
  (cond [(winning-board? (other-side (game-turn g)) (game-board g))
         (make-alert (string-append (side->string (other-side (game-turn g)))
                                    " won!")
                     g
                     (make-game X EMPTY-BOARD (game-player-types g)))]
        [else g]))

;; ----------------------------------------------------------------------------

;; Dealing with mouse events

;; handle-mouse : WorldState Integer Integer MouseEvent -> WorldState
(define (handle-mouse ws x y me)
  (cond [(mouse=? me "button-down")
         (handle-button-down ws x y)]
        [else ws]))

;; handle-button-down : WorldState Integer Integer -> WorldState
(define (handle-button-down ws x y)
  (cond [(game? ws)
         (cond [(not (equal? HUMAN (game-player-type ws)))
                (make-alert "it's not your move yet" ws ws)]
               [(not (valid-move? (game-board ws) (posn-x->column x)))
                (make-alert "invalid move: out of space" ws ws)]
               [else
                (check-winner (game-play-at ws (posn-x->column x)))])]
        [(alert? ws)
         ws]))

;; ----------------------------------------------------------------------------

;; Dealing with key events

;; handle-key : WorldState KeyEvent -> WorldState
(define (handle-key ws key)
  (cond [(key=? key "left") (handle-left ws)]
        [(key=? key "right") (handle-right ws)]
        [else ws]))

;; handle-left : WorldState -> WorldState
(define (handle-left ws)
  (cond [(game? ws) ws]
        [(alert? ws) (alert-prev ws)]))

;; handle-right : WorldState -> WorldState
(define (handle-right ws)
  (cond [(game? ws) (continue-game ws)]
        [(alert? ws) (alert-next ws)]))

;; continue-game : Game -> WorldState
(define (continue-game g)
  (continue-game/player-type
   (game-player-type g)
   g))

;; continue-game/player-type : PlayerType Game -> WorldState
(define (continue-game/player-type t g)
  (cond [(equal? t HUMAN) g]
        [else
         (local [(define next-moves
                   ((computer-next-moves t) (game-turn g) (game-board g)))]
           (cond
             [(empty? next-moves)
              (make-alert "no next moves" g g)]
             [else
              (check-winner (game-play-at g (random-element next-moves)))]))]))

;; ----------------------------------------------------------------------------

;; Drawing functions

;; draw-world : WorldState -> Image
(define (draw-world ws)
  (cond [(game? ws) (draw-game ws)]
        [(alert? ws) (overlay (draw-alert-box (alert-msg ws))
                              SEMI-TRANSPARENT-GRAY-RECTANGLE
                              (draw-game (alert-prev ws)))]))

;; draw-game : Game -> Image
(define (draw-game g)
  (above (draw-turn-msg (game-turn g))
         (draw-board (game-board g))))

;; draw-turn-msg : Side -> Image
(define (draw-turn-msg s)
  (overlay (text (string-append "It is " (side->string s) "'s turn")
                 TURN-MSG-FONT-SIZE
                 TURN-MSG-FONT-COLOR)
           TURN-MSG-BG))

;; draw-board : Board -> Image
(define (draw-board b)
  (place-images/align
   (build-list (* W H) (λ (i) (draw-space (board-ref b (index->pos i)))))
   (build-list (* W H) (λ (i) (pos->top-left-posn (index->pos i))))
   "left" "top"
   BOARD-BG))

;; draw-space : Space -> Image
(check-expect (draw-space #false) SQUARE)
(check-expect (draw-space X) X-SQUARE)
(check-expect (draw-space O) O-SQUARE)

(define (draw-space s)
  (cond [(false? s) SQUARE]
        [(equal? s X) X-SQUARE]
        [(equal? s O) O-SQUARE]))

;; index->pos : Index -> Pos
(check-expect (index->pos 0) (make-pos 0 0))
(check-expect (index->pos 5) (make-pos 5 0))
(check-expect (index->pos 7) (make-pos 0 1))
(check-expect (index->pos 10) (make-pos 3 1))
(check-expect (index->pos 41) (make-pos 6 5))

(define (index->pos i)
  (make-pos (remainder i W)
            (quotient i W)))

;; draw-alert-box : String -> Image
(define (draw-alert-box msg)
  (overlay (text msg ALERT-BOX-FONT-SIZE ALERT-BOX-FONT-COLOR)
           ALERT-BOX-BG))

;; ----------------------------------------------------------------------------

;; Functions relating screen coordinates to logical coordinates

;; pos->top-left-posn : Pos -> Posn
(define (pos->top-left-posn p)
  (make-posn (* SQUARE-SIZE (pos-column p))
             (* SQUARE-SIZE (- H (pos-row p) 1))))

;; posn-x->column : Number -> Natural
(check-expect (posn-x->column 0) 0)
(check-expect (posn-x->column 20) 0)
(check-expect (posn-x->column (* 1 SQUARE-SIZE)) 1)
(check-expect (posn-x->column (* 2 SQUARE-SIZE)) 2)
(check-expect (posn-x->column (* 5 SQUARE-SIZE)) 5)
(check-expect (posn-x->column (* 6 SQUARE-SIZE)) 6)
(check-expect (posn-x->column (* 7 SQUARE-SIZE)) 6)

(define (posn-x->column posn-x)
  (min (- W 1) (floor (/ posn-x SQUARE-SIZE))))

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

(define HUMAN-v-HUMAN (make-player-types HUMAN HUMAN))
(define HUMAN-v-COMPUTER (make-player-types HUMAN COMPUTER))
(define COMPUTER-v-HUMAN (make-player-types COMPUTER HUMAN))
(define COMPUTER-v-COMPUTER (make-player-types COMPUTER COMPUTER))

(define INIT-HH (make-game X EMPTY-BOARD HUMAN-v-HUMAN))
(define INIT-HC (make-game X EMPTY-BOARD HUMAN-v-COMPUTER))
(define INIT-CH (make-game X EMPTY-BOARD COMPUTER-v-HUMAN))
(define INIT-CC (make-game X EMPTY-BOARD COMPUTER-v-COMPUTER))

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

