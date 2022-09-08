#lang racket
(require "model.rkt"
         redex)

(define-language Unit-SEQ
  (e ::=
     unit
     (seq e e))
  (T Unit))

(define-union-language L+Γ+Unit
  Unit-SEQ L+Γ)

(define-extended-judgment-form
  L+Γ+Unit types
  #:mode (types+Unit I I O)
  #:contract (types+Unit Γ e T)
  [(types+Unit Γ e_1 Unit) (types+Unit Γ e_2 T_2)
   ---------------------------------------------- "T-SEQ"
   (types+Unit Γ (seq e_1 e_2) T_2)]

  [------------------------
   (types+Unit Γ unit Unit)])

(define (pretty-derivation+Unit d)
  (with-compound-rewriters
      (['seq (λ (lws) (list (list-ref lws 2) " ; " (list-ref lws 3)))]
       ['types+Unit (λ (lws) (list (list-ref lws 2) " ⊢ " (list-ref lws 3) ":" (list-ref lws 4)))])
    (pretty-derivation d)))

(module+ main
  (pretty-derivation+Unit
   (build-derivations
    (types+Unit
     (:+ (:+ ∅ a : Unit)
         b : Bool)
     (seq a (seq unit b))
     Bool))))
