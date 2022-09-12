#lang racket
(require redex
         pict
         (only-in unstable/gui/pict color))

(define (color-lw a-lw c)
  (struct-copy lw a-lw
               [e (color c (text (symbol->string (lw-e a-lw))))]))
