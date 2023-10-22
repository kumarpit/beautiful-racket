#lang racket

(require plai/test-harness)
(provide with-temporary-data-file
         read-from-string
         read-from-file
         read-from-string*
         read-from-file*)

(print-only-errors) ;; if uncommented, only failing tests are printed


;;
;; parsing.rkt - some tools for parsing lists of characters
;;

;;
;; WARNING: THE FOLLOWING FUNCTION DESIGN IS JUST HERE TO WRITE TESTS.
;;  YOU NEED NOT UNDERSTAND IT (but are welcome to)
;;

;; string (string -> X) -> X
;; call proc with the name of a temp file containing data, and yield its result
;; WARNING: NOT RE-ENTRANT!
(define (with-temporary-data-file data proc)
  (let ([tmp-path (make-temporary-file)])
    (dynamic-wind
     (λ () (call-with-output-file tmp-path
             (λ (out) (write-string data out))
             #:exists 'truncate))
     (λ () (proc (path->string tmp-path)))
     (λ () (delete-file tmp-path)))))


(test (with-temporary-data-file "Hello World!"
        (λ (tmp-file)
          (call-with-input-file tmp-file
            (λ (in) (string-append "Contents: " (port->string in))))))
      "Contents: Hello World!")


;; string -> s-expression
;; produce the first s-expression readable from the given string
(define (read-from-string s)
  (read (open-input-string s)))

(test (read-from-string "5") 5)
(test (read-from-string "(eat moar tacos)") (list 'eat 'moar 'tacos))
(test (read-from-string "(eat moar tacos)") '(eat moar tacos))
(test (read-from-string "{eat moar tacos}") '(eat moar tacos))


;; string -> s-expression
;; produce the first s-expression readable from the given file name's contents
(define (read-from-file filename)
  (read (open-input-file filename)))

(test (with-temporary-data-file "3\n"
        (λ (fname) (read-from-file fname)))
      3)

(test (with-temporary-data-file "3\n"
        (λ (fname) (read-from-file fname)))
      '3)

(test (with-temporary-data-file "cheese-burger\n"
        (λ (fname) (read-from-file fname)))
      'cheese-burger)

(test (with-temporary-data-file "{+ 3 7}\n"
        (λ (fname) (read-from-file fname)))
      '(+ 3 7))

(test (with-temporary-data-file "{+ {- 3 4} 7}\n"
        (λ (fname) (read-from-file fname)))
      (list '+ (list '- 3 4) 7))


;; Reading multiple datums
;; port -> (listof s-expression)
(define (read* port)
  (let loop ([datum (read port)])
    (if (eof-object? datum)
        empty
        (cons datum (loop (read port))))))


;; string -> (listof s-expression)
;; produce all the s-expressions readable from the given string
(define (read-from-string* s)
  (read* (open-input-string s)))

(test (read-from-string* "(eat moar tacos)") (list '(eat moar tacos)))
(test (read-from-string* "eat moar tacos") (list 'eat 'moar 'tacos))

;; string -> (listof s-expression)
;; produce all the s-expressions readable from the given file name's contents
(define (read-from-file* filename)
  (read* (open-input-file filename)))

(test (with-temporary-data-file "eat\n moar\n tacos\n"
        (λ (fname) (read-from-file* fname)))
      '(eat moar tacos))
