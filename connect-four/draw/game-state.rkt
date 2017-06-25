#lang htdp/isl+

(require "../util/provide.rkt")

(provide make-connect-four/gui
         WIDTH HEIGHT
         posn-x->column
         )

(require 2htdp/image)
(require 2htdp/universe)
(require turn-based-game/turn-based-game-gui)
(require "../util/generic.rkt")
(require "../util/list.rkt")

(require "../connect-four.rkt")
(require "color.rkt")

;; ----------------------------------------------------------------------------

(define SQUARE-SIZE 100)
(define CIRCLE-RADIUS 40)

(define FRAME-COLOR YELLOW)
(define SQUARE-OUTLINE-COLOR BROWN)
(define SQUARE (overlay (circle CIRCLE-RADIUS "solid" WHITE)
                        (square SQUARE-SIZE "outline" SQUARE-OUTLINE-COLOR)
                        (square SQUARE-SIZE "solid" FRAME-COLOR)))
(define X-SQUARE (overlay (circle CIRCLE-RADIUS "solid" RED) SQUARE))
(define O-SQUARE (overlay (circle CIRCLE-RADIUS "solid" BLACK) SQUARE))
(define X-GHOST-SQUARE (overlay (circle CIRCLE-RADIUS 100 RED) SQUARE))
(define O-GHOST-SQUARE (overlay (circle CIRCLE-RADIUS 100 BLACK) SQUARE))

(define BOARD-WIDTH (* W SQUARE-SIZE))
(define BOARD-HEIGHT (* H SQUARE-SIZE))
(define BOARD-BG (empty-scene BOARD-WIDTH BOARD-HEIGHT))

(define TURN-MSG-POSN (make-posn 0 0))
(define TURN-MSG-WIDTH BOARD-WIDTH)
(define TURN-MSG-HEIGHT 30)
(define TURN-MSG-FONT-SIZE 20)
(define TURN-MSG-FONT-COLOR BLACK)
(define TURN-MSG-BG (empty-scene TURN-MSG-WIDTH TURN-MSG-HEIGHT))

(define BOARD-IMG-POSN (make-posn 0 TURN-MSG-HEIGHT))

(define WIDTH (max TURN-MSG-WIDTH BOARD-WIDTH))
(define HEIGHT (+ TURN-MSG-HEIGHT BOARD-HEIGHT))

(define BACKGROUND-IMG (empty-scene WIDTH HEIGHT))

(define WINNER-MSG-FONT-SIZE 25)
(define WINNER-MSG-FONT-COLOR RED)
(define WINNER-MSG-BG (empty-scene (* 1/2 WIDTH) (* 1/5 HEIGHT)))

;; ----------------------------------------------------------------------------

(define-struct/generic connect-four/gui connect-four []
  #:methods gen:turn-based-game/gui
  [;; start-turn : TBGG -> TurnState
   (define (start-turn self) #false)

   ;; display-game-state : TBGG GameState Side TurnState -> Image
   (define (display-game-state self state side turn-state)
     (place-images/align
      (list (draw-turn-msg side)
            (draw-game-state state side turn-state))
      (list TURN-MSG-POSN
            BOARD-IMG-POSN)
      "left" "top"
      BACKGROUND-IMG))

   (define (display-end-state self state side winner)
     (overlay
      (draw-winner-msg winner)
      (display-game-state self state side (start-turn self))))

   ;; handle-mouse :
   ;; TBGG GameState Side TurnState Posn MouseEvent -> HandlerResult
   (define (handle-mouse self state side turn-state posn mouse)
     (define x_b (- (posn-x posn) (posn-x BOARD-IMG-POSN)))
     (define y_b (- (posn-y posn) (posn-y BOARD-IMG-POSN)))
     (cond [(and (<= 0 x_b) (< x_b BOARD-WIDTH)
                 (<= 0 y_b) (< y_b BOARD-HEIGHT))
            (cond
              [(mouse=? mouse "button-down")
               (finish-turn (quotient x_b SQUARE-SIZE))]
              [else
               (continue-turn (quotient x_b SQUARE-SIZE))])]
           [else (continue-turn turn-state)]))

   ;; handle-key :
   ;; TBGG GameState Side TurnState KeyEvent -> HandlerResult
   (define (handle-key self state side turn-state key)
     (cond
       [(key=? key "\r")
        (cond [turn-state (finish-turn turn-state)]
              [else (continue-turn turn-state)])]
       [else (continue-turn turn-state)]))])

;; ----------------------------------------------------------------------------

;; Drawing functions

;; draw-turn-msg : Side -> Image
(define (draw-turn-msg s)
  (overlay (text (string-append "It is " (side->string s) "'s turn")
                 TURN-MSG-FONT-SIZE
                 TURN-MSG-FONT-COLOR)
           TURN-MSG-BG))

;; draw-game-state : GameState -> Image
(define (draw-game-state state side turn-state)
  (cond [(and (not (false? turn-state))
              (board-valid-move? state turn-state))
         (local [(define row
                   (index-of (list-ref (board-columns state) turn-state)
                             #false))]
           (place-image/align
            (draw-ghost-space side)
            (* turn-state SQUARE-SIZE)
            (* (- H 1 row) SQUARE-SIZE)
            "left" "top"
            (draw-board state)))]
        [else
         (draw-board state)]))

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

;; draw-ghost-space : Space -> Image
(check-expect (draw-ghost-space #false) SQUARE)
(check-expect (draw-ghost-space X) X-GHOST-SQUARE)
(check-expect (draw-ghost-space O) X-GHOST-SQUARE)

(define (draw-ghost-space s)
  (cond [(false? s) SQUARE]
        [(equal? s X) X-GHOST-SQUARE]
        [(equal? s O) X-GHOST-SQUARE]))

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

