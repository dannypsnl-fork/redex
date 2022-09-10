#lang scribble/manual
@(require (for-label racket/base)
          "../stlc/model.rkt"
          redex)

@title{redex: type theory}
@author{Lîm Tsú-thuàn}


@section{simply typed lambda calculus}

The lambda calculus with simply type has the following rules(with base types and primitive forms).

@(pretty-judgment-form types)

We can check some derivation result @code{((λ (x Number) x) (+ 1 2))}.

@(pretty-derivation
  (build-derivations
   (types ∅
          ((λ (x Number) x) (+ 1 2))
          Number)))

We can check some derivation result @code{(f (if false true false))} where @code{f : (→ Bool Bool)}.

@(pretty-derivation
  (build-derivations
   (types
    (:+ ∅ f : (→ Bool Bool))
    (f (if false true false))
    Bool)))
