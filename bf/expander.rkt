#lang br/quicklang

;; an expander for the bf languae built using *functional principles*
;; as of my current understanding of functional programming - we want to avoid 
;; state and mutation as much as we can. we use accumulators to approximate state
;; in other words - rather than storing state values outside our functions,
;; we let them travel *through* our functions

;; @TODO

(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin
     'PARSE-TREE))
(provide (rename-out [bf-module-begin #%module-begin]))