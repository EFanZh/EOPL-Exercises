#lang eopl

;; Exercise 2.31 [★★] Sometimes it is useful to specify a concrete syntax as a sequence of symbols and integers,
;; surrounded by parentheses. For example, one might define the set of prefix lists by
;;
;;     Prefix-list ::= (Prefix-exp)
;;     Prefix-exp  ::= Int
;;                 ::= - Prefix-exp Prefix-exp
;;
;; so that (- - 3 2 - 4 - 12 7) is a legal prefix list. This is sometimes called Polish prefix notation, after its
;; inventor, Jan Łukasiewicz. Write a parser to convert a prefix-list to the abstract syntax
;;
;;     (define-datatype prefix-exp prefix-exp?
;;       (const-exp
;;        (num integer?))
;;       (diff-exp
;;        (operand1 prefix-exp?)
;;        (operand2 prefix-exp?)))
;;
;; so that the example above produces the same abstract syntax tree as the sequence of constructors
;;
;;     (diff-exp
;;      (diff-exp
;;       (const-exp 3)
;;       (const-exp 2))
;;      (diff-exp
;;       (const-exp 4)
;;       (diff-exp
;;        (const-exp 12)
;;        (const-exp 7))))
;;
;; As a hint, consider writing a procedure that takes a list and produces a prefix-exp and the list of leftover list
;; elements.

(define-datatype prefix-exp prefix-exp?
  [const-exp [num integer?]]
  [diff-exp [operand1 prefix-exp?]
            [operand2 prefix-exp?]])

(define parse-prefix-exp
  (lambda (prefix-list)
    (let ([head (car prefix-list)]
          [tail (cdr prefix-list)])
      (cond [(integer? head) (cons (const-exp head) tail)]
            [(eqv? head '-) (let* ([operand-1-and-rest-1 (parse-prefix-exp tail)]
                                   [operand-1 (car operand-1-and-rest-1)]
                                   [rest-1 (cdr operand-1-and-rest-1)]
                                   [operand-2-and-rest-2 (parse-prefix-exp rest-1)]
                                   [operand-2 (car operand-2-and-rest-2)]
                                   [rest-2 (cdr operand-2-and-rest-2)])
                              (cons (diff-exp operand-1 operand-2) rest-2))]
            [else (eopl:error 'parse-prefix-exp "Bad syntax: ~s." prefix-list)]))))

(define parse-prefix-list
  (lambda (prefix-list)
    (let* ([exp-and-rest (parse-prefix-exp prefix-list)]
           [exp (car exp-and-rest)]
           [rest (cdr exp-and-rest)])
      (if (null? rest)
          exp
          (eopl:error 'parse-prefix-list "Expect null after prefix-exp, but got: ~s." rest)))))

(provide const-exp diff-exp parse-prefix-list)
