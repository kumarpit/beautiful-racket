#lang br/quicklang
;; the purpose of stackerizer is to generate test programs for stacker
;; by converting racket s-exp to stacker programs

(provide + *)  ;; provide quicklang's definition of +, *

(define-macro (stackerizer-mb EXPR)  ;; rename macro #%module-begin to prevent clash with 
                                     ;; the one exported by quicklang
                                     ;; #' creates a thunk-ish like object, i.e preserves
                                     ;; lexical context
  #'(#%module-begin
        (for-each displayln (reverse (flatten EXPR)))))

(provide (rename-out [stackerizer-mb #%module-begin])) 

#;
(define-macro-cases +  ;; define-macro-cases matches on multiple patterns
  [(+ FIRST) #'FIRST]  ;; within syntax pattern, all-caps identifier doesn't match literally
  [(+ FIRST NEXT ...) #'(list '+ FIRST (+ NEXT ...))])

;; extracted further to support multiple arguments
;; a macro that makes other macros
#;
(define-macro (define-op OP)
  #'(define-macro-cases OP
      [(OP FIRST) #'FIRST]  ;; LHS -> syntax pattern  RHS -> syntax template
      [(OP FIRST NEXT (... ...))  ;; since we want the ... not to be used within define-op, we escape
       #'(list 'OP FIRST (OP NEXT (... ...)))]))

;; (define-op +)
;; (define-op *)

;; a pointless optimiation expressing the power of ... when used in a syntax template
(define-macro (define-ops OP ...) ; args stored in `OP ...`
  #'(begin
      (define-macro-cases OP ;; `OP` from `OP ...`
        [(OP FIRST) #'FIRST]
        [(OP FIRST NEXT (... ...))
         #'(list 'OP FIRST (OP NEXT (... ...)))])
      ...)) ;; `...` from `OP ...`

(define-ops + *)