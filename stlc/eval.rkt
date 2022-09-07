#lang racket
(require "model.rkt"
         redex
         redex/tut-subst)

(define-extended-language Ev L+Γ
  (p (e ...))
  (P (e ... E e ...))
  (E (v E) ; done
     (E e) ; to run
     (+ v ... E e ...)
     (if E e e)
     hole)
  ; value
  (v (λ (x t) e)
     number
     true
     false))


(define-metafunction Ev
  Σ : number ... -> number
  [(Σ number ...)
   ,(apply + (term (number ...)))])

(define-metafunction Ev
  subst : x v e -> e
  [(subst x v e)
   ,(subst/proc x? (list (term x)) (list (term v)) (term e))])
(define x? (redex-match Ev x))

(define red
  (reduction-relation
   Ev
   #:domain p
   (--> (in-hole P (if true e_1 e_2))
        (in-hole P e_1)
        "if-t")
   (--> (in-hole P (if false e_1 e_2))
        (in-hole P e_2)
        "if-f")
   (--> (in-hole P ((λ (x t) e) v))
        (in-hole P (subst x v e))
        "β-reduction")
   (--> (in-hole P (+ number ...))
        (in-hole P (Σ number ...))
        "plus")))

(module+ main
  (require pict)

  (scale (vl-append
          20
          (language->pict Ev)
          (reduction-relation->pict red))
         4/3))

(module+ test
  (test-->>
   red
   (term ((if false 2 3)))
   (term (3)))

  (test-results))
