#lang htdp/isl+

(require "util/provide.rkt")

(provide simulate-game display-simulation)

(require "connect-four.rkt")
(require "draw/game-state.rkt")
(require "util/list.rkt")
(require turn-based-game/turn-based-game)
(require turn-based-game/turn-based-game-gui)
(require turn-based-game/computer-player)
(require turn-based-game/computer-player/score-explore-random)
(require 2htdp/universe)
(require 2htdp/abstraction)

;; ----------------------------------------------------------------------------

;; A GameSequence is a [List-of GameState]
;; in chronological order

;; A GameHistory is a [List-of GameState]
;; in reverse-chronological order

;; A PlayerTypes is a (make-player-types ComputerPlayer ComputerPlayer)
(define-struct player-types [X O])

(define CC (make-player-types (computer/score-explore-random 2 20 20)
                              (computer/score-explore-random 2 20 20)))

;; A CompStates is a (make-comp-states CompState CompState)
(define-struct comp-states [X O])

;; comp-states-ref : CompStates Side -> CompState
(define (comp-states-ref cs s)
  (cond [(X? s) (comp-states-X cs)]
        [(O? s) (comp-states-O cs)]))

;; comp-states-set : CompStates Side CompState -> CompStates
(define (comp-states-set cs s c)
  (cond [(X? s) (make-comp-states c (comp-states-O cs))]
        [(O? s) (make-comp-states (comp-states-X cs) c)]))

;; comp-states-add-move : CompStates TBG Side Move -> CompStates
(define (comp-states-add-move cs tbg s m)
  (make-comp-states (comp-state-add-move (comp-states-X cs) tbg s m)
                    (comp-state-add-move (comp-states-O cs) tbg s m)))

;; ----------------------------------------------------------------------------

(define CONNECT-FOUR
  (make-connect-four/gui))

(define INIT
  (tbg-state CONNECT-FOUR
             (standard-initial-state CONNECT-FOUR)
             (standard-initial-side CONNECT-FOUR)))

;; simulate-game : TbgState PlayerTypes -> GameSequence
(define (simulate-game g p)
  (reverse
   (simulate-game-history g
                          (make-comp-states
                           (start-comp-state (player-types-X p))
                           (start-comp-state (player-types-O p)))
                          '())))

;; simulate-game-history : TbgState CompStates GameHistory -> GameHistory
;; accumulator history is a reverse-chronological list of previous
;;             game states
(define (simulate-game-history g p history)
  (match g
    [(tbg-state tbg b s)
     (cond
       ; trivial cases
       [(winning-state? tbg b X) (cons g history)]
       [(winning-state? tbg b O) (cons g history)]
       [else
        (local [(define c (comp-states-ref p s))
                (define c* (comp-state-next-state c tbg b s))
                (define mvs (comp-state-moves c* tbg b s))]
          (cond [(empty? mvs) (cons g history)]
                [else
                 (local [(define mv (random-element mvs))]
                   (simulate-game-history
                    (tbg-state-handle-move-choice g mv)
                    (comp-states-add-move (comp-states-set p s c*) tbg s mv)
                    (cons g history)))]))])]))

;; ----------------------------------------------------------------------------

;; display-simulation : GameSequence -> [List-of TbgState]
(define (display-simulation game-sequence)
  (big-bang game-sequence
    [on-tick rest 1]
    [to-draw (compose draw-game-state first)]
    [stop-when empty?]))

;; draw-game-state : TbgState -> Image
(define (draw-game-state g)
  (match g
    [(tbg-state tbg b s)
     (display-game-state tbg b s (start-turn tbg))]))

