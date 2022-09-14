#lang racket
(provide Ev
         red)
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
  (v (λ (x T) e)
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
        "IF-TRUE")
   (--> (in-hole P (if false e_1 e_2))
        (in-hole P e_2)
        "IF-FALSE")
   (--> (in-hole P ((λ (x T) e) v))
        (in-hole P (subst x v e))
        "βv")
   (--> (in-hole P (+ number ...))
        (in-hole P (Σ number ...))
        "Σ")))

(module+ main
  (require pict)

  (vl-append
   20
   (language->pict Ev)
   (reduction-relation->pict red)))

(module+ test
  (test-->>
   red
   (term (((λ (x Number) (+ x 2)) 3)))
   (term (5)))

  (test-->>
   red
   (term ((if false 2 3)))
   (term (3)))

  (test-->>
   red
   (term (((λ (y Number) y)
           ((λ (x Number) (+ x 2)) 3))))
   (term (5)))

  (test-results))
