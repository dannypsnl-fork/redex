#lang racket
(provide L+Γ
         types
         pretty-derivation
         pretty-judgment-form)
(require redex)

(define-language L
  (e ::=
     (e e)
     (λ (x T) e)
     x
     true
     false
     number
     (+ e ...)
     (if e e e))
  (T ::=
     (→ T T)
     ; Not part of the type theory, but Base type is required to make the system works
     Bool
     Number)
  (x variable-not-otherwise-mentioned))

(define-extended-language L+Γ L
  [Γ ::=
     ∅
     (:+ Γ x : T)])

(define-metafunction L+Γ
  [(different? x_1 x_1) #f]
  [(different? x_1 x_2) #t])

(define-judgment-form
  L+Γ
  #:mode (types I I O)
  #:contract (types Γ e T)
  [(types Γ e_1 (→ T_2 T_3)) (types Γ e_2 T_2)
   ------------------------------------------- "T-APP"
   (types Γ (e_1 e_2) T_3)]
  [(types (:+ Γ x : T_1) e T_2)
   ----------------------------------- "T-ABS"
   (types Γ (λ (x T_1) e) (→ T_1 T_2))]
  [--------------------- "T-VAR"
   (types (:+ Γ x : T) x T)]
  [(types Γ x_1 T_1) (side-condition (different? x_1 x_2))
   ------------------------------------
   (types (:+ Γ x_2 : T_2) x_1 T_1)]

  #| Base Types |#
  [(types Γ e Number) ...
   --------------------------
   (types Γ (+ e ...) Number)]
  [----------------------
   (types Γ number Number)]

  [-------------------
   (types Γ true Bool)]
  [-------------------
   (types Γ false Bool)]
  [(types Γ e_1 Bool) (types Γ e_2 T) (types Γ e_3 T)
   -----------------------------
   (types Γ (if e_1 e_2 e_3) T)])

(define (C thunk)
  (with-compound-rewriters
      ([':+ (λ (lws)
              (list (list-ref lws 2) ", "
                    (list-ref lws 3) ":" (list-ref lws 5)))]
       ['→ (λ (lws) (list (list-ref lws 2) (arrow->pict '->) (list-ref lws 3)))]
       ['types (λ (lws) (list (list-ref lws 2) " ⊢ " (list-ref lws 3) " : " (list-ref lws 4)))])
    (thunk)))

(define (pretty-judgment-form f)
  (C (λ () (judgment-form->pict types))))

(define (pretty-derivation d)
  (C (λ () (derivation->pict L (car d)))))

(module+ main
  (pretty-derivation
   (build-derivations
    (types ∅
           ((λ (x Number) x) (+ 1 2))
           Number)))

  (pretty-derivation
   (build-derivations
    (types
     (:+ ∅ f : (→ Bool Bool))
     (f (if false true false))
     Bool)))

  #;(pretty-derivation
     (build-derivations
      (types (:+ ∅ f : (→ Bool Bool))
             (λ (x : Bool) (f (if x false x)))
             (→ Bool Bool)))))
