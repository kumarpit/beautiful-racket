#lang plai

(require "../util/parsing.rkt")

;; Reverse Polish Math!
;; PLAI Scheme implementation of the "stacker" language,
;; based on concepts I learned in my "Definition of Programming
;; Languages" class and the PLAI book by Shriram Krishnamurthi

;; More on Reverse Polish Notation: https://en.wikipedia.org/wiki/Reverse_Polish_notation

(define-type RPN
  [num (n number?)]
  [add (l RPN?) (r RPN?)]
  [sub (l RPN?) (r RPN?)]
  [mul (l RPN?) (r RPN?)])
;; interp. program in the RPN language, corresponding to the following
;; Backus-Naur Form (BNF)-ish specification
;;  <RPN> ::= <num> 
;;          | <RPN> <RPN> +
;;          | <RPN> <RPN> -
;;          | <RPN> <RPN> *

(define RPN1 (num 1))
(define RPN2 (num -2))
(define RPN3 (add (num 1) (num 4)))
(define RPN4 (mul RPN1 (add RPN2 RPN3)))

;; RPN Template
#;
(define (fn-for-rpm rpn)
  (type-case RPN rpn
    [num (n) (...n)]
    [add (l r) (... (fn-for-rpn l)
                    (fn-for-rpn r))]
    [sub (l r) (... (fn-for-rpn l)
                    (fn-for-rpn r))]
    [mul (l r) (... (fn-for-rpn l)
                    (fn-for-rpn r))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Stack is (listof RPN)
;; interp. an implementation of the stack data structure
(define stack empty)

;; return most recently added element
(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

;; add to the beginning of the stack
(define (push-stack! arg)
  (set! stack (cons arg stack)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RPNFS (RPN-focused s-expression) is one of:
;; - Number
;; - `,RPNFS ,RPNFS +
;; - `,RPNFS ,RPNFS -
;; - `,RPNFS ,RPNFS *
;; - <any other s-expression>
;; interp.  a symbolic expression, but with a focus on those that
;; represent RPN expressions.

;; (listof Number or Symbol) -> RPN
;; produce a RPN value corresponding to the given RPN s-expression
;; Effect: 1) signals an error if the given s-exp does not represent a
;;            valid RPN
;;         2) modifies the stack
(define (parse/rpn rpn0)
  (local
    [(define (parse/rpn rpn)
       (if (empty? rpn)
           (pop-stack!)
           (let ([arg  (first rpn)]
                 [rst  (rest  rpn)])
             (cond
               [(number? arg)
                (push-stack! (num arg))]
               [(eq? '+ arg)
                (match-let ([`(,l . ,r) (check-empty-stack)])
                  (push-stack! (add l r)))]
               [(eq? '- arg) 
                (match-let ([`(,l . ,r) (check-empty-stack)])
                  (push-stack! (sub l r)))]
               [(eq? '* arg) 
                (match-let ([`(,l . ,r) (check-empty-stack)])
                  (push-stack! (mul l r)))]
               [else 
                (error "Bad RPN")])
             (parse/rpn rst))))

     ;; _ -> (RPN . RPN)
     ;; checks if one of last two items was empty
     ;; EFFECT: signals error if ^ true
     (define (check-empty-stack)
       (if (< (length stack) 2)
           (error 'parse/rpn "Bad RPN")
           `(,(pop-stack!) . ,(pop-stack!))))]
    
    (parse/rpn rpn0)))

(test (parse/rpn '(5)) (num 5))

(test (parse/rpn '(5 4 +))
      (add (num 4) (num 5)))

(test (parse/rpn '(3 4 + 2 * 9 -))
      (sub (num 9)
           (mul (num 2)
                (add
                 (num 4)
                 (num 3)))))

(test/exn (parse/rpn '(+ -)) "")
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RPN -> Number
;; produces the number to which the given RPN expression
;; evalutates to
(define (interp/rpn rpn)
  (type-case RPN rpn
    [num (n) n]
    [add (l r) (+ (interp/rpn l)
                  (interp/rpn r))]
    [sub (l r) (- (interp/rpn l)
                  (interp/rpn r))]
    [mul (l r) (* (interp/rpn l)
                  (interp/rpn r))]))

(test (interp/rpn
       (parse/rpn '(4)))
      4)

(test (interp/rpn
       (parse/rpn '(5 4 +)))
      9)

(test (interp/rpn
       (parse/rpn '(3 4 + 2 * 9 -)))
      -5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUTTING IT ALL TOGETHER  - an interpreter of files on disk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; string -> Number
;; produce the result of interpreting the AE stored in the file fname
;; EFFECT: signals an error if no file fname contains an AE representation
(define (interp-file fname)
  (interp/rpn
   (parse/rpn
    (read-from-file fname))))

(test (with-temporary-data-file "{2 3 4 + *}\n"
        (λ (fname) (interp-file fname)))
      14)

(test (with-temporary-data-file "{7 4 3 + - 1 *}\n"
        (λ (fname) (interp-file fname)))
      0)

(test (interp-file "stacker/test.rpn") 36)


              






