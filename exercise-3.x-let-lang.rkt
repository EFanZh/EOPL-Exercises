#lang eopl

;; This code is a implementation of an extended version of LET language.

(define-datatype expval expval?
  [num-val [value number?]]
  [bool-val [boolean boolean?]]
  [emptylist-val]
  [pair-val [car expval?]
            [cdr expval?]])

(define expval->num
  (lambda (v)
    (cases expval v
      [num-val (num) num]
      [else (expval-extractor-error 'num v)])))

(define expval->bool
  (lambda (v)
    (cases expval v
      [bool-val (bool) bool]
      [else (expval-extractor-error 'bool v)])))

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
    (extend-env 'i
                (num-val 1)
                (extend-env 'v
                            (num-val 5)
                            (extend-env 'x
                                        (num-val 10)
                                        (empty-env))))))

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
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    [expression ("minus" "(" expression ")") minus-exp]
    [expression ("+" "(" expression "," expression ")") add-exp]
    [expression ("*" "(" expression "," expression ")") mul-exp]
    [expression ("/" "(" expression "," expression ")") div-exp]
    [expression ("equal?" "(" expression "," expression ")") equal?-exp]
    [expression ("greater?" "(" expression "," expression ")") greater?-exp]
    [expression ("less?" "(" expression "," expression ")") less?-exp]
    [expression ("cons" "(" expression "," expression ")") cons-exp]
    [expression ("car" "(" expression ")") car-exp]
    [expression ("cdr" "(" expression ")") cdr-exp]
    [expression ("null?" "(" expression ")") null?-exp]
    [expression ("emptylist") emptylist-exp]
    [expression ("list" "(" (separated-list expression ",") ")") list-exp]
    [expression ("cond" (arbno expression "==>" expression) "end") cond-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
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
                                (bool-val #t)
                                (bool-val #f))))]
      [if-exp (exp1 exp2 exp3) (let ((val1 (value-of exp1 env)))
                                 (if (expval->bool val1)
                                     (value-of exp2 env)
                                     (value-of exp3 env)))]
      [let-exp (var exp1 body) (let ([val1 (value-of exp1 env)])
                                 (value-of body
                                           (extend-env var val1 env)))]
      [minus-exp (exp1) (num-val (- (expval->num (value-of exp1 env))))]
      [add-exp (exp1 exp2) (let ([val1 (value-of exp1 env)]
                                 [val2 (value-of exp2 env)])
                             (let ([num1 (expval->num val1)]
                                   [num2 (expval->num val2)])
                               (num-val (+ num1 num2))))]
      [mul-exp (exp1 exp2) (let ([val1 (value-of exp1 env)]
                                 [val2 (value-of exp2 env)])
                             (let ([num1 (expval->num val1)]
                                   [num2 (expval->num val2)])
                               (num-val (* num1 num2))))]
      [div-exp (exp1 exp2) (let ([val1 (value-of exp1 env)]
                                 [val2 (value-of exp2 env)])
                             (let ([num1 (expval->num val1)]
                                   [num2 (expval->num val2)])
                               (num-val (quotient num1 num2))))]
      [equal?-exp (exp1 exp2) (let ([val1 (value-of exp1 env)]
                                    [val2 (value-of exp2 env)])
                                (let ([num1 (expval->num val1)]
                                      [num2 (expval->num val2)])
                                  (bool-val (= num1 num2))))]
      [greater?-exp (exp1 exp2) (let ([val1 (value-of exp1 env)]
                                      [val2 (value-of exp2 env)])
                                  (let ([num1 (expval->num val1)]
                                        [num2 (expval->num val2)])
                                    (bool-val (> num1 num2))))]
      [less?-exp (exp1 exp2) (let ([val1 (value-of exp1 env)]
                                   [val2 (value-of exp2 env)])
                               (let ([num1 (expval->num val1)]
                                     [num2 (expval->num val2)])
                                 (bool-val (< num1 num2))))]
      [cons-exp (exp1 exp2) (let ([val1 (value-of exp1 env)]
                                  [val2 (value-of exp2 env)])
                              (pair-val val1 val2))]
      [car-exp (exp1) (cases expval (value-of exp1 env)
                        [pair-val (car _) car]
                        [else (eopl:error 'value-of "Not a pair")])]
      [cdr-exp (exp1) (cases expval (value-of exp1 env)
                        [pair-val (_ cdr) cdr]
                        [else (eopl:error 'value-of "Not a pair")])]
      [null?-exp (exp1) (cases expval (value-of exp1 env)
                          [emptylist-val () (bool-val #t)]
                          [else (bool-val #f)])]
      [emptylist-exp () (emptylist-val)]
      [list-exp (exps) (let ([vals (map (lambda (exp) (value-of exp env)) exps)])
                         (let loop ([vals vals])
                           (if (null? vals)
                               (emptylist-val)
                               (pair-val (car vals) (loop (cdr vals))))))]
      [cond-exp (exps1 exps2) (let loop ([exps1 exps1]
                                         [exps2 exps2])
                                (if (null? exps1)
                                    (eopl:error 'value-of "All cond tests failed.")
                                    (let ([condition (value-of (car exps1) env)])
                                      (if (expval->bool condition)
                                          (value-of (car exps2) env)
                                          (loop (cdr exps1)
                                                (cdr exps2))))))])))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide num-val bool-val emptylist-val pair-val run)
