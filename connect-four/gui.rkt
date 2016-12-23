#lang htdp/isl+

(require "util/provide.rkt")

(provide start-game)

(require 2htdp/image)
(require 2htdp/universe)

(require "connect-four.rkt")
(require "draw/color.rkt")
(require "draw/game-state.rkt")

;; ----------------------------------------------------------------------------

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

;; draw-alert-box : String -> Image
(define (draw-alert-box msg)
  (overlay (text msg ALERT-BOX-FONT-SIZE ALERT-BOX-FONT-COLOR)
           ALERT-BOX-BG))

