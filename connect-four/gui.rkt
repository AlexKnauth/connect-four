#lang htdp/isl+

(require "util/provide.rkt")

(provide start-game
         draw-game-state
         )

(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/abstraction)
(require "util/list.rkt")

(require "connect-four.rkt")

;; ----------------------------------------------------------------------------

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

(define WINNER-MSG-FONT-SIZE 25)
(define WINNER-MSG-FONT-COLOR RED)
(define WINNER-MSG-BG TURN-MSG-BG)

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

;; The main start-game function

;; A WorldState is one of:
;;  - GameState
;;  - Alert

;; An Alert is a (make-alert String GameState WorldState)
(define-struct alert (msg prev next))

;; A GameState is one of:
;;  - Game
;;  - EndState
;; game-state? : Any -> Boolean
(define (game-state? v)
  (or (game? v) (end-state? v)))

;; start-game : Game -> WorldState
(define (start-game start)
  (big-bang start
    [on-mouse handle-mouse]
    [on-key handle-key]
    [to-draw draw-world]))

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
  (cond [(game-state? ws) ws]
        [(alert? ws) (alert-prev ws)]))

;; handle-right : WorldState -> WorldState
(define (handle-right ws)
  (cond [(game-state? ws)
         (cond [(game? ws) (continue-game/alert ws)]
               [(end-state? ws) (reset-game ws)])]
        [(alert? ws) (alert-next ws)]))

;; continue-game/alert : Game -> WorldState
(define (continue-game/alert g)
  (local [(define g* (continue-game g))]
    (cond [(not (false? g*)) g*]
          [(end-state? g*) g*]
          [(equal? HUMAN (game-player-type g)) g]
          [else (make-alert "no next moves" g g)])))

;; reset-game : EndState -> Game
(define (reset-game e)
  (make-game X EMPTY-BOARD (game-player-types (end-state-game e))))

;; ----------------------------------------------------------------------------

;; Drawing functions

;; draw-world : WorldState -> Image
(define (draw-world ws)
  (cond [(game-state? ws) (draw-game-state ws)]
        [(alert? ws) (overlay (draw-alert-box (alert-msg ws))
                              SEMI-TRANSPARENT-GRAY-RECTANGLE
                              (draw-game-state (alert-prev ws)))]))

;; draw-game-state : GameState -> Image
(define (draw-game-state g)
  (cond [(game? g)
         (above (draw-turn-msg (game-turn g))
                (draw-board (game-board g)))]
        [(end-state? g)
         (above (draw-winner-msg (end-state-winner g))
                (draw-board (game-board (end-state-game g))))]))

;; draw-turn-msg : Side -> Image
(define (draw-turn-msg s)
  (overlay (text (string-append "It is " (side->string s) "'s turn")
                 TURN-MSG-FONT-SIZE
                 TURN-MSG-FONT-COLOR)
           TURN-MSG-BG))

;; draw-winner-msg : [Maybe Side] -> Image
(define (draw-winner-msg winner)
  (overlay (text (cond [(false? winner) "It's a tie!"]
                       [else (string-append (side->string winner) " won!")])
                 WINNER-MSG-FONT-SIZE
                 WINNER-MSG-FONT-COLOR)
           WINNER-MSG-BG))

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

