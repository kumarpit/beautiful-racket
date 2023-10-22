#lang br/quicklang

;; Every language made with racket has two essential components:
;; 1. the *reader*, converts source code to racket s-expressions
;; 2. the *expander*, which evaluates these s-expressions to produce native
;;    racket source code

;; The READER
;; Every reader must export the *read-syntax* function - which takes the path 
;; and port for reading the data (the source code)
;; it returns a *module*, packages as a *syntax object* (more on this later)

;; Modules
;; like everything in racket, a module is also an expression, which follows the following pattern
#;
(module module-name which-expander
  42        ;; the body of the module
  "foobar"  ;; contains expressions
  (+ 1 1)   ;; to expand and evaluate
  ...)

;; module-name: arbitrary, inconsequential
;; which-expander: either the name of in the installed language
;;                 or explicit path to a .rkt file



;; String Port -> Syntax Object
;; parse source code to s-expression representation
(define (read-syntax path port)
  (define src-lines (port->lines port))                        ;; src-lines is (listof String)
  (define src-datums (format-datums '(handle ~a) src-lines))   
  (define module-datum `(module stacker-mod "stacker.rkt" ,@src-datums))
  (datum->syntax #f module-datum))

(provide read-syntax)

(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...
     (display (first stack))))
(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)
(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)
(define (push-stack! arg)
  (set! stack (cons arg stack)))

(define (handle [arg #f])
  (cond
    [(number? arg) (push-stack! arg)]
    [(or (equal? + arg) (equal? * arg))
     (define op-result (arg (pop-stack!) (pop-stack!)))
     (push-stack! op-result)]))
(provide handle)

(provide + *)