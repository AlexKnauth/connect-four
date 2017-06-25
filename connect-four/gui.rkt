#lang htdp/isl+

(require "util/provide.rkt")

(provide start-game HH HC CC)

(require 2htdp/image)
(require 2htdp/universe)
(require turn-based-game/computer-player/score-explore-random)
(require turn-based-game/controller/computer-player-gui-controller)
(require "util/hash.rkt")

(require "connect-four.rkt")
(require "draw/color.rkt")
(require "draw/game-state.rkt")

;; ----------------------------------------------------------------------------

;; Data Definition

;; A PlayerTypes is a
;;   (make-player-types [Maybe ComputerPlayer] [Maybe ComputerPlayer])
(define-struct player-types [X O])

(define HH (make-player-types #false #false))
(define HC (make-player-types #false (computer/score-explore-random 2 20 20)))
(define CC (make-player-types (computer/score-explore-random 2 20 20)
                              (computer/score-explore-random 2 20 20)))

;; ----------------------------------------------------------------------------

;; The main start-game function

;; start-game : PlayerTypes -> WorldState
(define (start-game p)
  (start/computer (make-connect-four/gui)
                  (cond [(and (false? (player-types-X p))
                              (false? (player-types-O p)))
                         (hash)]
                        [(false? (player-types-X p))
                         (hash O (player-types-O p))]
                        [(false? (player-types-O p))
                         (hash X (player-types-X p))]
                        [else (hash X (player-types-X p)
                                    O (player-types-O p))])))

