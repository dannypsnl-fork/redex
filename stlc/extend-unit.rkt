#lang racket
(require "model.rkt"
         "eval.rkt"
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
  #:mode (⊢ I I O)
  #:contract (⊢ Γ e T)
  [(⊢ Γ e_1 Unit) (⊢ Γ e_2 T_2)
   ---------------------------------------------- "T-SEQ"
   (⊢ Γ (seq e_1 e_2) T_2)]

  [------------------------
   (⊢ Γ unit Unit)])

(define (pretty-derivation+Unit d)
  (with-compound-rewriters
      (['seq (λ (lws) (list (list-ref lws 2) " ; " (list-ref lws 3)))]
       ['⊢ (λ (lws) (list (list-ref lws 2) " ⊢ " (list-ref lws 3) ":" (list-ref lws 4)))])
    (pretty-derivation d)))

(module+ main
  (pretty-derivation+Unit
   (build-derivations
    (⊢
     (:+ (:+ ∅ a : Unit)
         b : Bool)
     (seq a (seq unit b))
     Bool))))

#|
extend evaluation
|#
(define-extended-language Ev-unit L+Γ+Unit
  (E ::=
     (seq E e)
     (seq v E)
     hole)
  (v ::=
     unit))

(define-union-language Ev+unit
  Ev-unit Ev)

(define red+unit
  (extend-reduction-relation
   red
   Ev+unit
   #:domain p
   (--> (in-hole P (seq unit e_2))
        (in-hole P e_2)
        "E-SEQNEXT")
   (--> (in-hole P unit)
        (in-hole P unit))))

(module+ main
  (require pict)

  (vl-append
   20
   (language->pict Ev-unit)
   (reduction-relation->pict red+unit))

  (traces red+unit
          (term ((seq unit 2))))

  (traces red+unit
          (term ((seq ((λ (x Number) unit) 1)
                      (seq unit 2))))))
