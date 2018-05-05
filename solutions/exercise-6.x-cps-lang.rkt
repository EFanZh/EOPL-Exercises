#lang eopl

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
    [expression ("+" "(" (separated-list expression ",") ")") sum-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression) "in" expression) letrec-exp]
    [expression (identifier) var-exp]
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    [expression ("proc" "(" (arbno identifier) ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

;; CPS-OUT grammar.

(define cps-out-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define cps-out-grammar
  '([cps-out-program (tfexp) cps-a-program]
    [simple-expression (number) cps-const-exp]
    [simple-expression (identifier) cps-var-exp]
    [simple-expression ("-" "(" simple-expression "," simple-expression ")") cps-diff-exp]
    [simple-expression ("zero?" "(" simple-expression ")") cps-zero?-exp]
    [simple-expression ("+" "(" (separated-list simple-expression ",") ")") cps-sum-exp]
    [simple-expression ("proc" "(" (arbno identifier) ")" tfexp) cps-proc-exp]
    [tfexp (simple-expression) simple-exp->exp]
    [tfexp ("let" identifier "=" simple-expression "in" tfexp) cps-let-exp]
    [tfexp ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" tfexp) "in" tfexp) cps-letrec-exp]
    [tfexp ("if" simple-expression "then" tfexp "else" tfexp) cps-if-exp]
    [tfexp ("(" simple-expression (arbno simple-expression) ")") cps-call-exp]))

(sllgen:make-define-datatypes cps-out-lexical-spec cps-out-grammar)

(define cps-out-scan&parse (sllgen:make-string-parser cps-out-lexical-spec cps-out-grammar))

;; Transformer.

(define list-set
  (lambda (lst n val)
    (cond [(null? lst) (eopl:error 'list-set "ran off end")]
          [(zero? n) (cons val (cdr lst))]
          [else (cons (car lst) (list-set (cdr lst) (- n 1) val))])))

(define fresh-identifier
  (let ([sn 0])
    (lambda (identifier)
      (set! sn (+ sn 1))
      (string->symbol (string-append (symbol->string identifier) "%" (number->string sn))))))

(define inp-exp-simple?
  (lambda (exp)
    (cases expression exp
      [const-exp (num) #t]
      [var-exp (var) #t]
      [diff-exp (exp1 exp2) (and (inp-exp-simple? exp1)
                                 (inp-exp-simple? exp2))]
      [zero?-exp (exp1)
                 (inp-exp-simple? exp1)]
      [proc-exp (ids exp) #t]
      [sum-exp (exps) (all-simple? exps)]
      [else #f])))

(define all-simple?
  (lambda (exps)
    (if (null? exps)
        #t
        (and (inp-exp-simple? (car exps))
             (all-simple? (cdr exps))))))

(define make-send-to-cont
  (lambda (cont bexp)
    (cps-call-exp cont (list bexp))))

(define cps-of-zero?-exp
  (lambda (exp1 k-exp)
    (cps-of-exps (list exp1)
                 (lambda (new-rands)
                   (make-send-to-cont k-exp (cps-zero?-exp (car new-rands)))))))

(define cps-of-sum-exp
  (lambda (exps k-exp)
    (cps-of-exps exps
                 (lambda (new-rands)
                   (make-send-to-cont k-exp (cps-sum-exp new-rands))))))

(define cps-of-diff-exp
  (lambda (exp1 exp2 k-exp)
    (cps-of-exps (list exp1 exp2)
                 (lambda (new-rands)
                   (make-send-to-cont k-exp (cps-diff-exp (car new-rands) (cadr new-rands)))))))

(define cps-of-if-exp
  (lambda (exp1 exp2 exp3 k-exp)
    (cps-of-exps (list exp1)
                 (lambda (new-rands)
                   (cps-if-exp (car new-rands) (cps-of-exp exp2 k-exp) (cps-of-exp exp3 k-exp))))))

(define cps-of-let-exp
  (lambda (id rhs body k-exp)
    (cps-of-exps (list rhs)
                 (lambda (new-rands)
                   (cps-let-exp id (car new-rands) (cps-of-exp body k-exp))))))

(define cps-of-letrec-exp
  (lambda (proc-names idss proc-bodies body k-exp)
    (cps-letrec-exp proc-names
                    (map (lambda (ids)
                           (append ids (list 'k%00)))
                         idss)
                    (map (lambda (exp)
                           (cps-of-exp exp (cps-var-exp 'k%00)))
                         proc-bodies)
                    (cps-of-exp body k-exp))))

(define cps-of-call-exp
  (lambda (rator rands k-exp)
    (cps-of-exps (cons rator rands)
                 (lambda (new-rands)
                   (cps-call-exp (car new-rands) (append (cdr new-rands) (list k-exp)))))))

(define report-invalid-exp-to-cps-of-simple-exp
  (lambda (exp)
    (eopl:error 'cps-simple-of-exp "non-simple expression to cps-of-simple-exp: ~s" exp)))

(define cps-of-simple-exp
  (lambda (exp)
    (cases expression exp
      [const-exp (num) (cps-const-exp num)]
      [var-exp (var) (cps-var-exp var)]
      [diff-exp (exp1 exp2) (cps-diff-exp (cps-of-simple-exp exp1) (cps-of-simple-exp exp2))]
      [zero?-exp (exp1) (cps-zero?-exp (cps-of-simple-exp exp1))]
      [proc-exp (ids exp) (cps-proc-exp (append ids (list 'k%00)) (cps-of-exp exp (cps-var-exp 'k%00)))]
      [sum-exp (exps) (cps-sum-exp (map cps-of-simple-exp exps))]
      [else (report-invalid-exp-to-cps-of-simple-exp exp)])))

(define cps-of-exp
  (lambda (exp cont)
    (cases expression exp
      [const-exp (num) (make-send-to-cont cont (cps-const-exp num))]
      [var-exp (var) (make-send-to-cont cont (cps-var-exp var))]
      [proc-exp (vars body) (make-send-to-cont cont
                                               (cps-proc-exp (append vars (list 'k%00))
                                                             (cps-of-exp body (cps-var-exp 'k%00))))]
      [zero?-exp (exp1) (cps-of-zero?-exp exp1 cont)]
      [diff-exp (exp1 exp2) (cps-of-diff-exp exp1 exp2 cont)]
      [sum-exp (exps) (cps-of-sum-exp exps cont)]
      [if-exp (exp1 exp2 exp3) (cps-of-if-exp exp1 exp2 exp3 cont)]
      [let-exp (var exp1 body) (cps-of-let-exp var exp1 body cont)]
      [letrec-exp (ids bidss proc-bodies body) (cps-of-letrec-exp ids bidss proc-bodies body cont)]
      [call-exp (rator rands) (cps-of-call-exp rator rands cont)])))

(define cps-of-exps
  (lambda (exps builder)
    (define list-index
      (lambda (pred lst)
        (cond [(null? lst) #f]
              [(pred (car lst)) 0]
              [(list-index pred (cdr lst)) => (lambda (n)
                                                (+ n 1))]
              [else #f])))
    (let cps-of-rest ([exps exps])
      (let ([pos (list-index (lambda (exp)
                               (not (inp-exp-simple? exp)))
                             exps)])
        (if (not pos)
            (builder (map cps-of-simple-exp exps))
            (let ([var (fresh-identifier 'var)])
              (cps-of-exp (list-ref exps pos)
                          (cps-proc-exp (list var) (cps-of-rest (list-set exps pos (var-exp var)))))))))))

(define cps-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (cps-a-program (cps-of-exps (list exp1)
                                                    (lambda (new-args)
                                                      (simple-exp->exp (car new-args)))))])))

;; Data structures - expressed values.

(define-datatype proc proc?
  [procedure [vars (list-of symbol?)]
             [body tfexp?]
             [env environment?]])

(define-datatype expval expval?
  [num-val (value number?)]
  [bool-val (boolean boolean?)]
  [proc-val (proc proc?)])

(define expval-extractor-error
  (lambda (variant value)
    [eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value]))

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

(define expval->proc
  (lambda (v)
    (cases expval v
      [proc-val (proc) proc]
      [else (expval-extractor-error 'proc v)])))

;; Data structure - environment.

(define empty-env
  (lambda ()
    '()))

(define extend-env*
  (lambda (syms vals old-env)
    (cons (list 'let syms vals) old-env)))

(define extend-env-rec**
  (lambda (p-names b-varss p-bodies saved-env)
    (cons (list 'letrec p-names b-varss p-bodies) saved-env)))

(define environment?
  (list-of (lambda (p)
             (and (pair? p) (or (eqv? (car p) 'let) (eqv? (car p) 'letrec))))))

(define apply-env
  (lambda (env search-sym)
    (define list-index
      (lambda (sym los)
        (let loop ([pos 0]
                   [los los])
          (cond [(null? los) #f]
                [(eqv? sym (car los)) pos]
                [else (loop (+ pos 1) (cdr los))]))))
    (if (null? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ([binding (car env)]
               [saved-env (cdr env)])
          (let ([pos (list-index search-sym (cadr binding))])
            (if pos
                (case (car binding)
                  [(let) (list-ref (caddr binding) pos)]
                  [(letrec) (let ([bvars (caddr binding)]
                                  [bodies (cadddr binding)])
                              (proc-val (procedure (list-ref bvars pos) (list-ref bodies pos) env)))])
                (apply-env saved-env search-sym)))))))

;; Data structure - continuation.

(define-datatype continuation continuation?
  [end-cont])

;; Interpreter.

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [end-cont () val])))

(define apply-procedure/k
  (lambda (proc1 args cont)
    (cases proc proc1
      [procedure (vars body saved-env) (value-of/k body (extend-env* vars args saved-env) cont)])))

(define value-of-simple-exp
  (lambda (exp env)
    (cases simple-expression exp
      [cps-const-exp (num) (num-val num)]
      [cps-var-exp (var) (apply-env env var)]
      [cps-diff-exp (exp1 exp2) (let ([val1 (expval->num (value-of-simple-exp exp1 env))]
                                      [val2 (expval->num (value-of-simple-exp exp2 env))])
                                  (num-val (- val1 val2)))]
      [cps-zero?-exp (exp1) (bool-val (zero? (expval->num (value-of-simple-exp exp1 env))))]
      [cps-sum-exp (exps) (let ([nums (map (lambda (exp)
                                             (expval->num (value-of-simple-exp exp env)))
                                           exps)])
                            (num-val (let sum-loop ([nums nums])
                                       (if (null? nums)
                                           0
                                           (+ (car nums) (sum-loop (cdr nums)))))))]
      [cps-proc-exp (vars body) (proc-val (procedure vars body env))])))

(define value-of/k
  (lambda (exp env cont)
    (cases tfexp exp
      [simple-exp->exp (simple) (apply-cont cont (value-of-simple-exp simple env))]
      [cps-let-exp (var rhs body) (let ([val (value-of-simple-exp rhs env)])
                                    (value-of/k body (extend-env* (list var) (list val) env) cont))]
      [cps-letrec-exp (p-names b-varss p-bodies letrec-body) (value-of/k letrec-body
                                                                         (extend-env-rec** p-names b-varss p-bodies env)
                                                                         cont)]
      [cps-if-exp (simple1 body1 body2) (if (expval->bool (value-of-simple-exp simple1 env))
                                            (value-of/k body1 env cont)
                                            (value-of/k body2 env cont))]
      [cps-call-exp (rator rands) (let ([rator-proc (expval->proc (value-of-simple-exp rator env))]
                                        [rand-vals (map (lambda (simple)
                                                          (value-of-simple-exp simple env))
                                                        rands)])
                                    (apply-procedure/k rator-proc rand-vals cont))])))

(define value-of-program
  (lambda (pgm)
    (cases cps-out-program pgm
      [cps-a-program (exp1) (value-of/k exp1 (empty-env) (end-cont))])))

;; Interface.

(define run
  (lambda (string)
    (let ([cpsed-pgm (cps-of-program (scan&parse string))])
      (value-of-program cpsed-pgm))))

(provide bool-val num-val run)
