#lang htdp/isl+

(require "provide.rkt")

(provide index-of list-set random-element)

;; index-of : ∀[X] [List-of X] X -> [Maybe Nat]
(check-expect (index-of (list) 'a) #false)
(check-expect (index-of (list 'a) 'a) 0)
(check-expect (index-of (list 'b 'c) 'a) #false)
(check-expect (index-of (list 'b 'c 'a 'x) 'a) 2)
(check-expect (index-of (list 'a 'b 'c 'd) 'x) #false)
(check-expect (index-of (list 'a 'b 'c 'd) 'a) 0)
(check-expect (index-of (list 'a 'b 'c 'd) 'b) 1)
(check-expect (index-of (list 'a 'b 'c 'd) 'c) 2)
(check-expect (index-of (list 'a 'b 'c 'd) 'd) 3)

(define (index-of lox x)
  (index-of/acc lox x 0))

;; index-of/acc : ∀[X] [List-of X] X Nat -> [Maybe Nat]
(define (index-of/acc lox x acc)
  (cond [(empty? lox) #false]
        [else
         (if (equal? x (first lox))
             acc
             (index-of/acc (rest lox) x (add1 acc)))]))

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

