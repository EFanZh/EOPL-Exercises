#lang eopl

;; Exercise 6.19 [★★] Write a Scheme procedure tail-form? that takes the syntax tree of a program in CPS-IN,
;; expressed in the grammar of figure 6.3, and determines whether the same string would be in tail form according to the
;; grammar of figure 6.5.

;; CPS-IN grammar.

(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define the-grammar
  '([program (expression) a-program]
    [expression (number) const-exp]
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression) "in" expression) letrec-exp]
    [expression (identifier) var-exp]
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    [expression ("proc" "(" (arbno identifier) ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

(define all
  (lambda (pred items)
    (let loop ([items items])
      (or (null? items)
          (and (pred (car items))
               (loop (cdr items)))))))

(define simple-exp?
  (lambda (exp)
    (cases expression exp
      [const-exp (num) #t]
      [diff-exp (exp1 exp2) (and (simple-exp? exp1)
                                 (simple-exp? exp2))]
      [zero?-exp (exp1) (simple-exp? exp1)]
      [var-exp (var) #t]
      [proc-exp (vars body) (tail-form-exp? body)]
      [else #f])))

(define tail-form-exp?
  (lambda (exp)
    (cases expression exp
      [const-exp (num) #t]
      [diff-exp (exp1 exp2) (and (simple-exp? exp1)
                                 (simple-exp? exp2))]
      [zero?-exp (exp1) (simple-exp? exp1)]
      [if-exp (exp1 exp2 exp3) (and (simple-exp? exp1)
                                    (tail-form-exp? exp2)
                                    (tail-form-exp? exp3))]
      [var-exp (var) #t]
      [let-exp (var exp1 body) (and (simple-exp? exp1)
                                    (tail-form-exp? body))]
      [letrec-exp (p-names b-varss p-bodies letrec-body) (and (all tail-form-exp? p-bodies)
                                                              (tail-form-exp? letrec-body))]
      [proc-exp (vars body) (tail-form-exp? body)]
      [call-exp (rator rands) (and (simple-exp? rator) (all simple-exp? rands))])))

(define tail-form?
  (lambda (pgm)
    (cases program pgm
      [a-program (exp) (tail-form-exp? exp)])))

(provide scan&parse tail-form?)
