#lang htdp/isl+

(require "connect-four.rkt")
(require "draw/game-state.rkt")
(require 2htdp/universe)

;; A GameSequence is a [List-of GameState]
;; in chronological order

;; A GameHistory is a [List-of GameState]
;; in reverse-chronological order

;; simulate-game : GameState -> GameSequence
(define (simulate-game g)
  (reverse (simulate-game-history g '())))

;; simulate-game-history : GameState GameHistory -> GameHistory
;; accumulator history is a reverse-chronological list of previous
;;             game states
(define (simulate-game-history g history)
  (cond
    ; trivial case
    [(end-state? g) (cons g history)]
    [(equal? HUMAN (game-player-type g))
     (error "simulate-game: cannot simulate a human")]
    [else
     (local [(define g* (continue-game g))]
       (cond [(false? g*) (error "simulate-game: no progress made")]
             [else (simulate-game-history g* (cons g history))]))]))

;; ----------------------------------------------------------------------------

;; display-simulation : GameSequence -> [List-of Game]
(define (display-simulation game-sequence)
  (big-bang game-sequence
    [on-tick rest 1]
    [to-draw (compose draw-game-state first)]
    [stop-when empty?]))

