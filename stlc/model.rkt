#lang racket
(require redex)

(define-language L
  (e (e e) (λ (x T) e) x true false number (+ e ...) (if e e e) (fix e))
  (T (→ T T) Bool Number)
  (x variable-not-otherwise-mentioned))

(define-extended-language L+Γ L [Γ ∅ (x : T Γ)])

(define-metafunction L+Γ [(different? x_1 x_1) #f] [(different? x_1 x_2) #t])
(define-judgment-form
  L+Γ
  #:mode (types I I O)
  #:contract (types Γ e T)
  [(types Γ e_1 (→ T_2 T_3)) (types Γ e_2 T_2)
   -------------------------
   (types Γ (e_1 e_2) T_3)]
  [(types (x : T_1 Γ) e T_2)
   -----------------------------------
   (types Γ (λ (x T_1) e) (→ T_1 T_2))]
  [---------------------
   (types (x : T Γ) x T)]
  [(types Γ x_1 T_1)
   (side-condition (different? x_1 x_2))
   ------------------------------------
   (types (x_2 : T_2 Γ) x_1 T_1)]
  [(types Γ e Number) ...
   -----------------------
   (types Γ (+ e ...) Number)]
  [--------------------
   (types Γ number Number)]
  [--------------------
   (types Γ true Bool)]
  [--------------------
   (types Γ false Bool)]
  [(types Γ e_1 Bool)
   (types Γ e_2 T)
   (types Γ e_3 T)
   -----------------------------
   (types Γ (if e_1 e_2 e_3) T)])

(define (pretty-derivation d)
  (with-compound-rewriters
      (['→ (λ (lws) (list (list-ref lws 2) " → " (list-ref lws 3)))]
       ['types (λ (lws) (list (list-ref lws 2) " ⊢ " (list-ref lws 3) " : " (list-ref lws 4)))])
    (derivation->pict L (car d))))

(pretty-derivation (build-derivations (types ∅ ((λ (x Number) x) (+ 1 2)) Number)))

(pretty-derivation (build-derivations (types (f : (→ Bool Bool) ∅) (f (if false true false)) Bool)))

#;(pretty-derivation
   (build-derivations (types (f : (→ Bool Bool) ∅) (λ (x : Bool) (f (if x false x))) (→ Bool Bool))))
