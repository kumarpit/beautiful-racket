#lang br/quicklang
(require "parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module bf-mod "expander.rkt"
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)

;; a *token* is the smallest meaningful chunk of a string of a source code
;; a source string is converted to a bunch of tokens before it is passed to 
;; the parser by something called the *tokenizer*
;; the tokenizer is entirely optional - if we do not use a tokenizer then every
;; character that appears in our source string must be handled by the parser
;; (i.e) must be a part of the grammar since we just built a grammar-based parser
;; the benefits of tokenizing are summarized below

;; 1. strings in the source code that are mean­ing­less—e.g., those 
;;    that repre­sent comments—can be removed 

;; 2. strings that repre­sent a type of value—e.g., those that repre­sent 
;;    numbers—can be labeled with a generic token type, like NUMBER. This simpli­fies
;;    the grammar, as we can just use NUMBER to mean any number string, ;
;;    rather than having to make grammar rules that cover every possible number pattern,
;;    e.g., 23.8, 3/4, 42+3i

;; 3. strings that should be handled liter­ally—e.g., a single char­acter like
;;    < repre­senting an oper­a­tion—can just pass through

(require brag/support)
(define (make-tokenizer port)
  (define (next-token)  ;; the next-token function tokenizes the entire source file
    (define bf-lexer
      (lexer            ;; lexer comes from brag/support
       [(char-set "><-.,+[]") lexeme]
       [any-char (next-token)]))  ;; ignore anything other than the char-set
    (bf-lexer port))  
  next-token)