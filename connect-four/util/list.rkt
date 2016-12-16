#lang htdp/isl+

(require "provide.rkt")

(provide list-set random-element)

;; list-set : ∀[X] [List-of X] Natural X -> [List-of X]
;; Assume that the list has an ith element (that means it must have
;; at least i+1 elements)
(check-expect (list-set (list 'a 'b 'c 'd) 0 'X)
              (list 'X 'b 'c 'd))
(check-expect (list-set (list 'a 'b 'c 'd) 2 'X)
              (list 'a 'b 'X 'd))

(define (list-set lox i x)
  (cond
    [(zero? i) (cons x (rest lox))]
    [else (cons (first lox) (list-set (rest lox) (sub1 i) x))]))

;; random-element : ∀[X] [NEList-of X] -> X
(check-random (random-element (list 'a 'b 'c 'd))
              (list-ref (list 'a 'b 'c 'd) (random 4)))

(define (random-element lox)
  (list-ref lox (random (length lox))))

