#lang htdp/isl+

(require "../util/provide.rkt")

(provide draw-game-state
         WIDTH HEIGHT
         posn-x->column
         )

(require 2htdp/image)

(require "../connect-four.rkt")
(require "color.rkt")

;; ----------------------------------------------------------------------------

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

;; ----------------------------------------------------------------------------

;; Drawing functions

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

