#lang eopl

;; Exercise 3.13 [★] Change the values of the language so that integers are the only expressed values. Modify if so
;; that the value 0 is treated as false and all other values are treated as true. Modify the predicates accordingly.

(define-datatype expval expval?
  [num-val [value number?]])

(define expval->num
  (lambda (v)
    (cases expval v
      [num-val (num) num]
      [else (expval-extractor-error 'num v)])))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors
                "Looking for a ~s, found ~s"
                variant
                value)))

(define empty-env-record
  (lambda ()
    '()))

(define extended-env-record
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))
  
(define empty-env-record? null?)
  
(define environment?
  (lambda (x)
    (or (empty-env-record? x)
        (and (pair? x)
             (symbol? (car (car x)))
             (expval? (cadr (car x)))
             (environment? (cdr x))))))

(define extended-env-record->sym
  (lambda (r)
    (car (car r))))

(define extended-env-record->val
  (lambda (r)
    (cadr (car r))))

(define extended-env-record->old-env
  (lambda (r)
    (cdr r)))

(define init-env 
  (lambda ()
    (empty-env)))

(define empty-env
  (lambda ()
    (empty-env-record)))
  
(define empty-env? 
  (lambda (x)
    (empty-env-record? x)))

(define extend-env
  (lambda (sym val old-env)
    (extended-env-record sym val old-env)))

(define apply-env
  (lambda (env search-sym)
    (if (empty-env? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let ([sym (extended-env-record->sym env)]
              [val (extended-env-record->val env)]
              [old-env (extended-env-record->old-env env)])
          (if (eqv? search-sym sym)
              val
              (apply-env old-env search-sym))))))

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
    [expression (identifier) var-exp]
    [expression ("let" identifier "=" expression "in" expression) let-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
  
(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (value-of exp1 (init-env))])))

(define value-of
  (lambda (exp env)
    (cases expression exp
      [const-exp (num) (num-val num)]
      [var-exp (var) (apply-env env var)]
      [diff-exp (exp1 exp2) (let ([val1 (value-of exp1 env)]
                                  [val2 (value-of exp2 env)])
                              (let ([num1 (expval->num val1)]
                                    [num2 (expval->num val2)])
                                (num-val (- num1 num2))))]
      [zero?-exp (exp1) (let ([val1 (value-of exp1 env)])
                          (let ([num1 (expval->num val1)])
                            (if (zero? num1)
                                (num-val 1)
                                (num-val 0))))]
      [if-exp (exp1 exp2 exp3) (let* ([val1 (value-of exp1 env)])
                                 (if (zero? (expval->num val1))
                                     (value-of exp3 env)
                                     (value-of exp2 env)))]
      [let-exp (var exp1 body) (let ([val1 (value-of exp1 env)])
                                 (value-of body
                                           (extend-env var val1 env)))])))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide num-val run)
