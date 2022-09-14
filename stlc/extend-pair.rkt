#lang racket
(require "model.rkt"
         "eval.rkt"
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

#|
extend evaluation
|#
(define-extended-language Ev-pair L+Γ+Pair
  (E ::=
     (cons v E)
     (cons E e)
     (fst E)
     (snd E)
     hole))

(define-union-language Ev+pair
  Ev-pair Ev)

(define red-pair
  (extend-reduction-relation
   red
   Ev+pair
   #:domain p
   (--> (in-hole P (fst (cons e_1 e_2)))
        (in-hole P e_1)
        "E-PROJ1")
   (--> (in-hole P (snd (cons e_1 e_2)))
        (in-hole P e_2)
        "E-PROJ2")))

(module+ main
  (require pict)

  (vl-append
   20
   (language->pict Ev-pair)
   (reduction-relation->pict red-pair))

  (traces red-pair
          (term ((snd (fst (cons (cons 1 3) 2)))))))
