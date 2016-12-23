#lang htdp/isl

(require "../util/provide.rkt")

(provide BLACK WHITE RED YELLOW BROWN SEMI-TRANSPARENT-GRAY)

(require 2htdp/image)

;; ----------------------------------------------------------------------------

;; Constants for the Colors

(define BLACK "black")
(define WHITE "white")
(define RED "red")
(define YELLOW "yellow")
(define BROWN "brown")
(define SEMI-TRANSPARENT-GRAY (make-color 127 127 127 127))

