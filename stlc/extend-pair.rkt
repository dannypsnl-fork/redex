#lang racket
(require "model.rkt"
         redex)

(define-language L+Pair
  (e ::=
     (cons e e)
     (fst e)
     (snd e))
  (T (Pair T T)))

(define-union-language L+Γ+Pair
  L+Pair L+Γ)

(define-extended-judgment-form
  L+Γ+Pair types
  #:mode (⊢ I I O)
  #:contract (⊢ Γ e T)
  [(⊢ Γ e_1 T_1) (⊢ Γ e_2 T_2)
   ---------------------------------------------- "T-PAIR"
   (⊢ Γ (cons e_1 e_2) (Pair T_1 T_2))]

  [(⊢ Γ e (Pair T_1 T_2))
   ---------------------- "T-PROJ1"
   (⊢ Γ (fst e) T_1)]

  [(⊢ Γ e (Pair T_1 T_2))
   ---------------------- "T-PROJ2"
   (⊢ Γ (snd e) T_2)])

(define (pretty-derivation+Unit d)
  (with-compound-rewriters
      (['cons (λ (lws) (list "(" (list-ref lws 2) ", " (list-ref lws 3) ")"))]
       ['Pair (λ (lws) (list (list-ref lws 2) " × " (list-ref lws 3)))]
       ['⊢ (λ (lws) (list (list-ref lws 2) " ⊢ " (list-ref lws 3) ":" (list-ref lws 4)))])
    (pretty-derivation d)))

(module+ main
  (pretty-derivation+Unit
   (build-derivations
    (⊢
     (:+ (:+ ∅ a : Number)
         b : Bool)
     (fst (cons a b))
     Number))))
