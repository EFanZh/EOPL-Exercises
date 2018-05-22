#lang eopl

;; Exercise 6.38 [★★★] If a variable never appears on the left-hand side of a set expression, then it is immutable,
;; and could be treated as simple. Extend your solution to the preceding exercise so that all such variables are treated
;; as simple.

;; CPS-IN grammar.

(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define the-grammar
  '([program (expression) a-program]
    [var-definition ("%mut" identifier) mut-var-def]
    [var-definition ("%const" identifier) const-var-def]
    [expression (number) const-exp]
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("+" "(" (separated-list expression ",") ")") sum-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression) "in" expression) letrec-exp]
    [expression ("%letrec" (arbno var-definition "(" (arbno var-definition) ")" "=" expression) "in" expression)
                %letrec-exp]
    [expression (identifier) var-exp]
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    [expression ("%let" var-definition "=" expression "in" expression) %let-exp]
    [expression ("proc" "(" (arbno identifier) ")" expression) proc-exp]
    [expression ("%proc" "(" (arbno var-definition) ")" expression) %proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("set" identifier "=" expression) assign-exp]
    [expression ("print" "(" expression ")") print-exp]
    [expression ("newref" "(" expression ")") newref-exp]
    [expression ("deref" "(" expression ")") deref-exp]
    [expression ("setref" "(" expression "," expression ")") setref-exp]))

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
    [tfexp ("(" simple-expression (arbno simple-expression) ")") cps-call-exp]
    [tfexp ("printk" "(" simple-expression ")" ";" tfexp) cps-printk-exp]
    [tfexp ("newrefk" "(" simple-expression "," simple-expression ")") cps-newrefk-exp]
    [tfexp ("derefk" "(" simple-expression "," simple-expression ")") cps-derefk-exp]
    [tfexp ("setrefk" "(" simple-expression "," simple-expression ")" ";" tfexp) cps-setrefk-exp]))

(sllgen:make-define-datatypes cps-out-lexical-spec cps-out-grammar)

;; Mark const variable definitions.

(define never
  (lambda (s)
    (eopl:error s "Not gonna happen.")))

(define filter
  (lambda (pred lst)
    (let loop ([acc '()]
               [lst lst])
      (if (null? lst)
          (reverse acc)
          (let ([head (car lst)]
                [tail (cdr lst)])
            (if (pred head)
                (loop (cons head acc) tail)
                (loop acc tail)))))))

(define list-diffv
  (lambda (lhs rhs)
    (filter (lambda (x)
              (not (memv x rhs)))
            lhs)))

(define mark-var-defs
  (lambda (vars mut-vars)
    (map (lambda (var)
           (if (memv var mut-vars)
               (mut-var-def var)
               (const-var-def var)))
         vars)))

(define mark-const-def-exp
  (lambda (exp cont)
    (cases expression exp
      [const-exp (num) (cont exp '())]
      [var-exp (var) (cont exp '())]
      [proc-exp (vars body) (mark-const-def-exp body
                                                (lambda (new-body mut-vars)
                                                  ; Mark all procedure parameters as mutable because we may not know
                                                  ; the actual bound type when calling a precedure.
                                                  (cont (%proc-exp (map mut-var-def vars) new-body)
                                                        (list-diffv mut-vars vars))))]
      [%proc-exp (vars body) (never 'mark-const-def-exp)]
      [zero?-exp (exp1) (mark-const-def-exp exp1
                                            (lambda (new-exp mut-vars)
                                              (cont (zero?-exp new-exp) mut-vars)))]
      [diff-exp (exp1 exp2) (mark-const-def-exps (list exp1 exp2)
                                                 (lambda (new-exps mut-vars)
                                                   (cont (diff-exp (car new-exps) (cadr new-exps)) mut-vars)))]
      [sum-exp (exps) (mark-const-def-exps exps
                                           (lambda (new-exps mut-vars)
                                             (cont (sum-exp new-exps) mut-vars)))]
      [if-exp (exp1 exp2 exp3) (mark-const-def-exps (list exp1 exp2 exp3)
                                                    (lambda (new-exps mut-vars)
                                                      (cont (if-exp (car new-exps) (cadr new-exps) (caddr new-exps))
                                                            mut-vars)))]
      [let-exp (var exp1 body) (mark-const-def-exp exp1
                                                   (lambda (new-exp1 mut-vars1)
                                                     (mark-const-def-exp body
                                                                         (lambda (new-body mut-vars2)
                                                                           (if (memv var mut-vars2)
                                                                               (cont (%let-exp (mut-var-def var)
                                                                                               new-exp1
                                                                                               new-body)
                                                                                     (append mut-vars1
                                                                                             (list-diffv mut-vars2
                                                                                                         (list var))))
                                                                               (cont (%let-exp (const-var-def var)
                                                                                               new-exp1
                                                                                               new-body)
                                                                                     (append mut-vars1 mut-vars2)))))))]
      [%let-exp (var exp1 body) (never 'mark-const-def-exp)]
      [letrec-exp (ids bidss proc-bodies body)
                  (let loop ([acc-bidss '()]
                             [acc-proc-bodies '()]
                             [acc-mut-vars '()]
                             [bidss bidss]
                             [proc-bodies proc-bodies])
                    (if (null? bidss)
                        (mark-const-def-exp body
                                            (lambda (new-body mut-vars2)
                                              (let ([all-mut-vars (append acc-mut-vars mut-vars2)])
                                                (cont (%letrec-exp (mark-var-defs ids all-mut-vars)
                                                                   (reverse acc-bidss)
                                                                   (reverse acc-proc-bodies)
                                                                   new-body)
                                                      (list-diffv all-mut-vars ids)))))
                        (mark-const-def-exp (car proc-bodies)
                                            (lambda (new-proc-body mut-vars1)
                                              (loop (cons (map mut-var-def (car bidss)) acc-bidss)
                                                    (cons new-proc-body acc-proc-bodies)
                                                    (append (list-diffv mut-vars1 (car bidss)) acc-mut-vars)
                                                    (cdr bidss)
                                                    (cdr proc-bodies))))))]
      [%letrec-exp (ids bidss proc-bodies body) (never 'mark-const-def-exp)]
      [call-exp (rator rands) (mark-const-def-exps (cons rator rands)
                                                   (lambda (new-exps mut-vars)
                                                     (cont (call-exp (car new-exps) (cdr new-exps)) mut-vars)))]
      [assign-exp (var exp1) (mark-const-def-exp exp1
                                                 (lambda (new-exp mut-vars)
                                                   (cont (assign-exp var new-exp) (cons var mut-vars))))]
      [print-exp (rator) (mark-const-def-exp rator
                                             (lambda (new-exp mut-vars)
                                               (cont (print-exp new-exp) mut-vars)))]
      [newref-exp (exp1) (mark-const-def-exp exp1
                                             (lambda (new-exp mut-vars)
                                               (cont (newref-exp new-exp) mut-vars)))]
      [deref-exp (exp1) (mark-const-def-exp exp1
                                            (lambda (new-exp mut-vars)
                                              (cont (deref-exp new-exp) mut-vars)))]
      [setref-exp (exp1 exp2) (mark-const-def-exps (list exp1 exp2)
                                                   (lambda (new-exps mut-vars)
                                                     (cont (setref-exp (car new-exps) (cadr new-exps)) mut-vars)))])))

(define mark-const-def-exps
  (lambda (exps cont)
    (let loop ([acc-new-exps '()]
               [acc-mut-vars '()]
               [exps exps])
      (if (null? exps)
          (cont (reverse acc-new-exps) acc-mut-vars)
          (mark-const-def-exp (car exps)
                              (lambda (new-exp mut-vars)
                                (loop (cons new-exp acc-new-exps)
                                      (append mut-vars acc-mut-vars)
                                      (cdr exps))))))))

(define mark-const-def-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (mark-const-def-exp exp1
                                            (lambda (new-exp mut-args)
                                              (if (null? mut-args)
                                                  (a-program new-exp)
                                                  (never 'mark-const-def-program))))])))

;; Transformer.

(define fresh-identifier
  (let ([sn 0])
    (lambda (identifier)
      (set! sn (+ sn 1))
      (string->symbol (string-append (symbol->string identifier) "%" (number->string sn))))))

(define is-const
  (lambda (var senv)
    (let loop ([senv senv])
      (if (null? senv)
          #t ; Special case to handle false branch of an if expression.
          (cases var-definition (car senv)
            [const-var-def (var1) (if (eqv? var1 var)
                                      #t
                                      (loop (cdr senv)))]
            [mut-var-def (var1) (if (eqv? var1 var)
                                    #f
                                    (loop (cdr senv)))])))))

(define unmark-var-def
  (lambda (var)
    (cases var-definition var
      [const-var-def (var) var]
      [mut-var-def (var) var])))

(define unmark-var-defs
  (lambda (vars)
    (map unmark-var-def vars)))

(define all-simple?
  (lambda (exps senv)
    (if (null? exps)
        #t
        (and (inp-exp-simple? (car exps) senv)
             (all-simple? (cdr exps) senv)))))

(define inp-exp-simple?
  (lambda (exp senv)
    (cases expression exp
      [const-exp (num) #t]
      [var-exp (var) (is-const var senv)]
      [diff-exp (exp1 exp2) (and (inp-exp-simple? exp1 senv) (inp-exp-simple? exp2 senv))]
      [zero?-exp (exp1) (inp-exp-simple? exp1 senv)]
      [%proc-exp (ids exp) #t]
      [sum-exp (exps) (all-simple? exps senv)]
      [else #f])))

(define make-send-to-cont
  (lambda (cont bexp)
    (cps-call-exp cont (list bexp))))

(define cps-of-zero?-exp
  (lambda (exp1 senv k-exp)
    (cps-of-exps (list exp1)
                 senv
                 (lambda (new-rands senv)
                   (make-send-to-cont k-exp (cps-zero?-exp (car new-rands)))))))

(define cps-of-sum-exp
  (lambda (exps senv k-exp)
    (cps-of-exps exps
                 senv
                 (lambda (new-rands senv)
                   (make-send-to-cont k-exp (cps-sum-exp new-rands))))))

(define cps-of-diff-exp
  (lambda (exp1 exp2 senv k-exp)
    (cps-of-exps (list exp1 exp2)
                 senv
                 (lambda (new-rands senv)
                   (make-send-to-cont k-exp (cps-diff-exp (car new-rands) (cadr new-rands)))))))

(define cps-of-if-exp
  (lambda (exp1 exp2 exp3 senv k-exp)
    (cps-of-exps (list exp1)
                 senv
                 (lambda (new-rands senv)
                   (cps-if-exp (car new-rands) (cps-of-exp exp2 senv k-exp) (cps-of-exp exp3 senv k-exp))))))

(define cps-of-%let-exp
  (lambda (id rhs body senv k-exp)
    (cps-of-exps (list rhs)
                 senv
                 (lambda (new-rands senv)
                   (cases var-definition id
                     [const-var-def (var) (cps-let-exp var (car new-rands) (cps-of-exp body (cons id senv) k-exp))]
                     [mut-var-def (var) (cps-newrefk-exp (car new-rands)
                                                         (cps-proc-exp (list var)
                                                                       (cps-of-exp body (cons id senv) k-exp)))])))))

(define split-%letrec-defs
  (lambda (proc-names idss proc-bodies cont)
    (let loop ([acc-const-proc-names '()]
               [acc-const-idss '()]
               [acc-const-proc-bodies '()]
               [acc-mut-proc-names '()]
               [acc-mut-idss '()]
               [acc-mut-proc-bodies '()]
               [proc-names proc-names]
               [idss idss]
               [proc-bodies proc-bodies])
      (if (null? proc-names)
          (cont (reverse acc-const-proc-names)
                (reverse acc-const-idss)
                (reverse acc-const-proc-bodies)
                (reverse acc-mut-proc-names)
                (reverse acc-mut-idss)
                (reverse acc-mut-proc-bodies))
          (cases var-definition (car proc-names)
            [const-var-def (var) (loop (cons (car proc-names) acc-const-proc-names)
                                       (cons (car idss) acc-const-idss)
                                       (cons (car proc-bodies) acc-const-proc-bodies)
                                       acc-mut-proc-names
                                       acc-mut-idss
                                       acc-mut-proc-bodies
                                       (cdr proc-names)
                                       (cdr idss)
                                       (cdr proc-bodies))]
            [mut-var-def (var) (loop acc-const-proc-names
                                     acc-const-idss
                                     acc-const-proc-bodies
                                     (cons (car proc-names) acc-mut-proc-names)
                                     (cons (car idss) acc-mut-idss)
                                     (cons (car proc-bodies) acc-mut-proc-bodies)
                                     (cdr proc-names)
                                     (cdr idss)
                                     (cdr proc-bodies))])))))

(define make-places
  (lambda (names senv builder)
    (if (null? names)
        (builder senv)
        (let ([name (car names)])
          (cps-newrefk-exp (cps-const-exp 678)
                           (cps-proc-exp (list name)
                                         (make-places (cdr names) (cons (mut-var-def name) senv) builder)))))))

(define make-const-rec-procs
  (lambda (proc-names idss proc-bodies senv builder)
    (let ([letrec-senv (append proc-names senv)])
      (cps-letrec-exp (unmark-var-defs proc-names)
                      (map (lambda (ids)
                             (append (unmark-var-defs ids) (list 'k%00)))
                           idss)
                      (map (lambda (proc-body)
                             (cps-of-exp proc-body
                                         (cons (const-var-def 'k%00) (append (car idss) letrec-senv))
                                         (cps-var-exp 'k%00)))
                           proc-bodies)
                      (builder letrec-senv)))))

(define make-set-rec-procs
  (lambda (proc-names idss proc-bodies senv builder)
    (if (null? proc-names)
        (builder senv)
        (cps-setrefk-exp (cps-var-exp (unmark-var-def (car proc-names)))
                         (cps-proc-exp (append (unmark-var-defs (car idss)) (list 'k%00))
                                       (cps-of-exp (car proc-bodies)
                                                   (cons (const-var-def 'k%00) (append (car idss) senv))
                                                   (cps-var-exp 'k%00)))
                         (make-set-rec-procs (cdr proc-names)
                                             (cdr idss)
                                             (cdr proc-bodies)
                                             senv
                                             builder)))))

(define cps-of-%letrec-exp
  (lambda (proc-names idss proc-bodies body senv k-exp)
    (split-%letrec-defs proc-names
                        idss
                        proc-bodies
                        (lambda (const-proc-names const-idss const-proc-bodies mut-proc-names mut-idss mut-proc-bodies)
                          (make-places (unmark-var-defs mut-proc-names)
                                       senv
                                       (lambda (senv1)
                                         (make-const-rec-procs const-proc-names
                                                               const-idss
                                                               const-proc-bodies
                                                               senv1
                                                               (lambda (senv2)
                                                                 (make-set-rec-procs mut-proc-names
                                                                                     mut-idss
                                                                                     mut-proc-bodies
                                                                                     senv2
                                                                                     (lambda (senv3)
                                                                                       (cps-of-exp body
                                                                                                   senv3
                                                                                                   k-exp)))))))))))

(define cps-of-call-exp
  (lambda (rator rands senv k-exp)
    (cps-of-exps (cons rator rands)
                 senv
                 (lambda (new-rands senv)
                   (let loop ([acc '()]
                              [rands (append (cdr new-rands))])
                     (if (null? rands)
                         (cps-call-exp (car new-rands) (reverse (cons k-exp acc)))
                         (let ([var (fresh-identifier 'var)])
                           (cps-newrefk-exp (car rands)
                                            (cps-proc-exp (list var)
                                                          (loop (cons (cps-var-exp var) acc)
                                                                (cdr rands)))))))))))

(define cps-of-assign-exp
  (lambda (var exp1 senv k-exp)
    (cps-of-exps (list exp1)
                 senv
                 (lambda (new-rands senv)
                   (cps-setrefk-exp (cps-var-exp var) (car new-rands) (make-send-to-cont k-exp (cps-const-exp 27)))))))

(define report-invalid-exp-to-cps-of-simple-exp
  (lambda (exp)
    (eopl:error 'cps-of-simple-exp "non-simple expression to cps-of-simple-exp: ~s" exp)))

(define cps-of-simple-exp
  (lambda (exp senv)
    (cases expression exp
      [const-exp (num) (cps-const-exp num)]
      [var-exp (var) (if (is-const var senv)
                         (cps-var-exp var)
                         (never 'cps-of-simple-exp))]
      [diff-exp (exp1 exp2) (cps-diff-exp (cps-of-simple-exp exp1 senv) (cps-of-simple-exp exp2 senv))]
      [zero?-exp (exp1) (cps-zero?-exp (cps-of-simple-exp exp1 senv))]
      [%proc-exp (ids exp) (cps-proc-exp (append (unmark-var-defs ids) (list 'k%00))
                                         (cps-of-exp exp
                                                     (cons (const-var-def 'k%00) (append ids senv))
                                                     (cps-var-exp 'k%00)))]
      [sum-exp (exps) (cps-sum-exp (map (lambda (exp)
                                          (cps-of-simple-exp exp senv))
                                        exps))]
      [else (report-invalid-exp-to-cps-of-simple-exp exp)])))

(define cps-of-exp
  (lambda (exp senv k-exp)
    (cases expression exp
      [const-exp (num) (make-send-to-cont k-exp (cps-const-exp num))]
      [var-exp (var) (if (is-const var senv)
                         (make-send-to-cont k-exp (cps-var-exp var))
                         (cps-derefk-exp (cps-var-exp var) k-exp))]
      [proc-exp (vars body) (never 'cps-of-exp)]
      [%proc-exp (vars body) (make-send-to-cont k-exp
                                                (cps-proc-exp (append (unmark-var-defs vars) (list 'k%00))
                                                              (cps-of-exp body
                                                                          (cons (const-var-def 'k%00)
                                                                                (append vars senv))
                                                                          (cps-var-exp 'k%00))))]
      [zero?-exp (exp1) (cps-of-zero?-exp exp1 senv k-exp)]
      [diff-exp (exp1 exp2) (cps-of-diff-exp exp1 exp2 senv k-exp)]
      [sum-exp (exps) (cps-of-sum-exp exps senv k-exp)]
      [if-exp (exp1 exp2 exp3) (cps-of-if-exp exp1 exp2 exp3 senv k-exp)]
      [let-exp (var exp1 body) (never 'cps-of-exp)]
      [%let-exp (var exp1 body) (cps-of-%let-exp var exp1 body senv k-exp)]
      [letrec-exp (ids bidss proc-bodies body) (never 'cps-of-exp)]
      [%letrec-exp (ids bidss proc-bodies body) (cps-of-%letrec-exp ids bidss proc-bodies body senv k-exp)]
      [call-exp (rator rands) (cps-of-call-exp rator rands senv k-exp)]
      [assign-exp (var exp1) (cps-of-assign-exp var exp1 senv k-exp)]
      [print-exp (rator) (cps-of-exps (list rator)
                                      senv
                                      (lambda (simples senv)
                                        (cps-printk-exp (car simples) (make-send-to-cont k-exp (cps-const-exp 38)))))]
      [newref-exp (exp1) (cps-of-exps (list exp1)
                                      senv
                                      (lambda (simples senv)
                                        (cps-newrefk-exp (car simples) k-exp)))]
      [deref-exp (exp1) (cps-of-exps (list exp1)
                                     senv
                                     (lambda (simples senv)
                                       (cps-derefk-exp (car simples) k-exp)))]
      [setref-exp (exp1 exp2) (cps-of-exps (list exp1 exp2)
                                           senv
                                           (lambda (simples senv)
                                             (cps-setrefk-exp (car simples)
                                                              (cadr simples)
                                                              (make-send-to-cont k-exp (cps-const-exp 23)))))])))

(define cps-of-exps
  ; Actually the implementation is incorrect, because it can not handle some programs with special varible scopes.
  ; An issue has been filed here: https://github.com/mwand/eopl3/issues/19.
  (lambda (exps senv builder)
    (let cps-of-rest ([exps exps]
                      [senv senv]
                      [acc '()])
      (cond [(null? exps) (builder (reverse acc) senv)]
            [(inp-exp-simple? (car exps) senv) (cps-of-rest (cdr exps)
                                                            senv
                                                            (cons (cps-of-simple-exp (car exps) senv) acc))]
            [else (let ([var (fresh-identifier 'var)])
                    (cps-of-exp (car exps)
                                senv
                                (cps-proc-exp (list var)
                                              (cps-of-rest (cdr exps)
                                                           (cons (const-var-def var) senv)
                                                           (cons (cps-var-exp var) acc)))))]))))

(define cps-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (cps-a-program (cps-of-exps (list exp1)
                                                    '()
                                                    (lambda (new-args senv)
                                                      (simple-exp->exp (car new-args)))))])))

;; Data structures - expressed values.

(define-datatype proc proc?
  [procedure [vars (list-of symbol?)]
             [body tfexp?]
             [env environment?]])

(define-datatype expval expval?
  [num-val [value number?]]
  [bool-val [boolean boolean?]]
  [proc-val [proc proc?]]
  [ref-val [ref reference?]])

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value)))

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

(define expval->ref
  (lambda (v)
    (cases expval v
      [ref-val (ref) ref]
      [else (expval-extractor-error 'reference v)])))

;; Data structures - environment.

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

;; Data structures - continuation.

(define-datatype continuation continuation?
  [end-cont])

;; Data structures - store.

(define reference?
  (lambda (v)
    (integer? v)))

(define the-store 'uninitialized)

(define empty-store
  (lambda ()
    '()))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define newref
  (lambda (val)
    (let ([next-ref (length the-store)])
      (set! the-store (append the-store (list val)))
      next-ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref "illegal reference ~s in store ~s" ref the-store)))

(define setref!
  (lambda (ref val)
    (set! the-store (letrec ([setref-inner (lambda (store1 ref1)
                                             (cond [(null? store1) (report-invalid-reference ref the-store)]
                                                   [(zero? ref1) (cons val (cdr store1))]
                                                   [else (cons (car store1) (setref-inner (cdr store1) (- ref1 1)))]))])
                      (setref-inner the-store ref)))))

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
      [simple-exp->exp (bexp) (apply-cont cont (value-of-simple-exp bexp env))]
      [cps-let-exp (var exp1 body) (let ([val (value-of-simple-exp exp1 env)])
                                     (value-of/k body (extend-env* (list var) (list val) env) cont))]
      [cps-letrec-exp (p-names b-varss p-bodies letrec-body) (value-of/k letrec-body
                                                                         (extend-env-rec** p-names b-varss p-bodies env)
                                                                         cont)]
      [cps-if-exp (exp1 exp2 exp3) (value-of/k (if (expval->bool (value-of-simple-exp exp1 env))
                                                   exp2
                                                   exp3)
                                               env
                                               cont)]
      [cps-call-exp (rator rands) (let ([rator-proc (expval->proc (value-of-simple-exp rator env))]
                                        [rand-vals (map (lambda (bexp)
                                                          (value-of-simple-exp bexp env))
                                                        rands)])
                                    (apply-procedure/k rator-proc rand-vals cont))]
      [cps-printk-exp (simple body) (begin (eopl:printf "~s~%" (value-of-simple-exp simple env))
                                           (value-of/k body env cont))]
      [cps-newrefk-exp (simple1 simple2) (let ([val1 (value-of-simple-exp simple1 env)]
                                               [val2 (value-of-simple-exp simple2 env)])
                                           (let ([newval (ref-val (newref val1))])
                                             (apply-procedure/k (expval->proc val2) (list newval) cont)))]
      [cps-derefk-exp (simple1 simple2) (apply-procedure/k (expval->proc (value-of-simple-exp simple2 env))
                                                           (list (deref (expval->ref (value-of-simple-exp simple1
                                                                                                          env))))
                                                           cont)]
      [cps-setrefk-exp (simple1 simple2 body) (let ([ref (expval->ref (value-of-simple-exp simple1 env))]
                                                    [val (value-of-simple-exp simple2 env)])
                                                (begin (setref! ref val)
                                                       (value-of/k body env cont)))])))

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases cps-out-program pgm
      [cps-a-program (body) (value-of/k body (empty-env) (end-cont))])))

;; Interface.

(define run
  (lambda (string)
    (let* ([marked-pgm (mark-const-def-program (scan&parse string))]
           [cpsed-pgm (cps-of-program marked-pgm)])
      (value-of-program cpsed-pgm))))

(provide bool-val num-val run)
