#lang agile

(provide define-struct/generic define)

(require racket/splicing)

(begin-for-syntax
  (define-splicing-syntax-class struct-methods-decl
    [pattern (~seq #:methods interface:id [method-def:expr ...])
      #:with [norm ...]
      #'[#:methods interface [(splicing-local [] method-def ...)]]]))

(define-syntax-parser define-struct/generic
  [(define-struct/generic name:id [field:id ...]
     methods-decl:struct-methods-decl ...)
   #'(define-struct name [field ...]
       methods-decl.norm ... ...)]
  [(define-struct/generic name:id super:id [field:id ...]
     methods-decl:struct-methods-decl ...)
   #'(define-struct (name super) [field ...]
       methods-decl.norm ... ...)])

