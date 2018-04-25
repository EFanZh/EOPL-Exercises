#lang eopl

;; Exercise 5.49 [★] Do exercise 5.15 (continuations as a stack of frames) for THREADS.

;; Grammar.

(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define the-grammar
  '([program (expression) a-program]
    [expression (number) const-exp]
    [expression ("[" (separated-list number ",") "]") const-list-exp]
    [expression (identifier) var-exp]
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression ("proc" "(" identifier ")" expression) proc-exp]
    [expression ("(" expression expression ")") call-exp]
    [expression ("begin" expression (arbno ";" expression) "end") begin-exp]
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    [expression ("letrec" (arbno identifier "(" identifier ")" "=" expression) "in" expression) letrec-exp]
    [expression ("set" identifier "=" expression) set-exp]
    [expression ("spawn" "(" expression ")") spawn-exp]
    [expression ("yield" "(" ")") yield-exp]
    [expression ("mutex" "(" ")") mutex-exp]
    [expression ("wait" "(" expression ")") wait-exp]
    [expression ("signal" "(" expression ")") signal-exp]
    [expression (unop "(" expression ")") unop-exp]
    [unop ("car") car-unop]
    [unop ("cdr") cdr-unop]
    [unop ("null?") null?-unop]
    [unop ("zero?") zero?-unop]
    [unop ("print") print-unop]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

;; Data structures.

;; References.

(define reference?
  (lambda (v)
    (integer? v)))

;; Store.

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
    (set! the-store
          (letrec ([setref-inner (lambda (store1 ref1)
                                   (cond [(null? store1) (report-invalid-reference ref the-store)]
                                         [(zero? ref1) (cons val (cdr store1))]
                                         [else (cons (car store1) (setref-inner (cdr store1) (- ref1 1)))]))])
            (setref-inner the-store ref)))))

;; Procedures.

(define-datatype proc proc?
  [procedure [bvar symbol?]
             [body expression?]
             [env environment?]])

(define fresh-identifier
  (let ([sn 0])
    (lambda (identifier)
      (set! sn (+ sn 1))
      (string->symbol (string-append (symbol->string identifier) "%" (number->string sn))))))

;; Queue.

(define empty-queue
  (lambda ()
    '()))

(define empty? null?)

(define enqueue
  (lambda (q val)
    (append q (list val))))

(define dequeue
  (lambda (q f)
    (f (car q) (cdr q))))

;; Scheduler.

(define the-ready-queue 'uninitialized)
(define the-final-answer 'uninitialized)
(define the-max-time-slice 'uninitialized)
(define the-time-remaining 'uninitialized)

(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice)))

(define place-on-ready-queue!
  (lambda (th)
    (set! the-ready-queue (enqueue the-ready-queue th))))

(define time-expired?
  (lambda ()
    (zero? the-time-remaining)))

(define decrement-timer!
  (lambda ()
    (set! the-time-remaining (- the-time-remaining 1))))

(define set-final-answer!
  (lambda (val)
    (set! the-final-answer val)))

(define run-next-thread
  (lambda ()
    (if (empty? the-ready-queue)
        the-final-answer
        (dequeue the-ready-queue
                 (lambda (first-ready-thread other-ready-threads)
                   (set! the-ready-queue other-ready-threads)
                   (set! the-time-remaining the-max-time-slice)
                   (first-ready-thread))))))

;; Mutexes.

(define-datatype mutex mutex?
  [a-mutex [ref-to-closed? reference?]
           [ref-to-wait-queue reference?]])

(define new-mutex
  (lambda ()
    (a-mutex (newref #f) (newref '()))))

(define wait-for-mutex
  (lambda (m th)
    (cases mutex m
      [a-mutex (ref-to-closed? ref-to-wait-queue)
               (cond [(deref ref-to-closed?) (setref! ref-to-wait-queue (enqueue (deref ref-to-wait-queue) th))
                                             (run-next-thread)]
                     [else (setref! ref-to-closed? #t)
                           (th)])])))

(define signal-mutex
  (lambda (m th)
    (cases mutex m
      [a-mutex (ref-to-closed? ref-to-wait-queue) (let ([closed? (deref ref-to-closed?)]
                                                        [wait-queue (deref ref-to-wait-queue)])
                                                    (when closed?
                                                      (if (empty? wait-queue)
                                                          (setref! ref-to-closed? #f)
                                                          (dequeue wait-queue
                                                                   (lambda (first-waiting-th other-waiting-ths)
                                                                     (place-on-ready-queue! first-waiting-th)
                                                                     (setref! ref-to-wait-queue other-waiting-ths)))))
                                                    (th))])))

;; Expressed values.

(define-datatype expval expval?
  [num-val [value number?]]
  [bool-val [boolean boolean?]]
  [proc-val [proc proc?]]
  [list-val [lst (list-of expval?)]]
  [mutex-val [mutex mutex?]])

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

(define expval->list
  (lambda (v)
    (cases expval v
      [list-val (lst) lst]
      [else (expval-extractor-error 'list v)])))

(define expval->mutex
  (lambda (v)
    (cases expval v
      [mutex-val (l) l]
      [else (expval-extractor-error 'mutex v)])))

;; Environment.

(define environment?
  (list-of (lambda (p)
             (and (pair? p) (or (symbol? (car p)) ((list-of symbol?) (car p)))))))

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define extend-env-rec*
  (lambda (p-names b-vars p-bodies saved-env)
    (cons (list p-names b-vars p-bodies) saved-env)))

(define locate
  (lambda (sym los)
    (let loop ((pos 0) (los los))
      (cond [(null? los) #f]
            [(eqv? sym (car los)) pos]
            [else (loop (+ pos 1) (cdr los))]))))

(define apply-env
  (lambda (env search-sym)
    (if (null? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ((binding (car env))
               (saved-env (cdr env)))
          (if (symbol? (car binding))
              (if (eqv? search-sym (car binding))
                  (cadr binding)
                  (apply-env saved-env search-sym))
              (let ([pos (locate search-sym (car binding))]
                    [b-vars (cadr binding)]
                    [p-bodies (caddr binding)])
                (if pos
                    (newref (proc-val (procedure (list-ref b-vars pos) (list-ref p-bodies pos) env)))
                    (apply-env saved-env search-sym))))))))

;; Continuation.

(define-datatype frame frame?
  [end-main-thread-frame]
  [end-subthread-frame]
  [diff1-frame [exp2 expression?]
               [env environment?]]
  [diff2-frame [val1 expval?]]
  [if-test-frame [exp2 expression?]
                 [exp3 expression?]
                 [env environment?]]
  [rator-frame [rand expression?]
               [env environment?]]
  [rand-frame [val1 expval?]]
  [set-rhs-frame [loc reference?]]
  [spawn-frame]
  [wait-frame]
  [signal-frame]
  [unop-arg-frame [unop1 unop?]])

;; Interpreter.

(define apply-unop
  (lambda (unop1 arg cont)
    (cases unop unop1
      [zero?-unop () (apply-cont cont (bool-val (zero? (expval->num arg))))]
      [car-unop () (let ([lst (expval->list arg)])
                     (apply-cont cont (car lst)))]
      [cdr-unop () (let ([lst (expval->list arg)])
                     (apply-cont cont (list-val (cdr lst))))]
      [null?-unop () (apply-cont cont (bool-val (null? (expval->list arg))))]
      [print-unop () (begin (eopl:printf "~a~%" (expval->num arg))
                            (apply-cont cont (num-val 1)))])))

(define apply-procedure
  (lambda (proc1 arg cont)
    (cases proc proc1
      [procedure (var body saved-env) (value-of/k body (extend-env var (newref arg) saved-env) cont)])))

(define apply-cont
  (lambda (cont val)
    (if (time-expired?)
        (begin (place-on-ready-queue! (lambda ()
                                        (apply-cont cont val)))
               (run-next-thread))
        (begin (decrement-timer!)
               (cases frame (car cont)
                 [end-main-thread-frame ()
                                        (set-final-answer! val)
                                        (run-next-thread)]
                 [end-subthread-frame () (run-next-thread)]
                 [diff1-frame (exp2 saved-env) (value-of/k exp2 saved-env (cons (diff2-frame val) (cdr cont)))]
                 [diff2-frame (val1) (let ([n1 (expval->num val1)]
                                           [n2 (expval->num val)])
                                       (apply-cont (cdr cont) (num-val (- n1 n2))))]
                 [if-test-frame (exp2 exp3 env) (if (expval->bool val)
                                                    (value-of/k exp2 env (cdr cont))
                                                    (value-of/k exp3 env (cdr cont)))]
                 [rator-frame (rand saved-env) (value-of/k rand saved-env (cons (rand-frame val) (cdr cont)))]
                 [rand-frame (val1) (let ([proc (expval->proc val1)])
                                      (apply-procedure proc val (cdr cont)))]
                 [set-rhs-frame (loc) (begin (setref! loc val)
                                             (apply-cont (cdr cont) (num-val 26)))]
                 [spawn-frame () (let ([proc1 (expval->proc val)])
                                   (place-on-ready-queue! (lambda ()
                                                            (apply-procedure proc1
                                                                             (num-val 28)
                                                                             (list (end-subthread-frame)))))
                                   (apply-cont (cdr cont) (num-val 73)))]
                 [wait-frame () (wait-for-mutex (expval->mutex val)
                                                (lambda ()
                                                  (apply-cont (cdr cont) (num-val 52))))]
                 [signal-frame () (signal-mutex (expval->mutex val)
                                                (lambda ()
                                                  (apply-cont (cdr cont) (num-val 53))))]
                 [unop-arg-frame (unop1) (apply-unop unop1 val (cdr cont))])))))

(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      [const-exp (num) (apply-cont cont (num-val num))]
      [const-list-exp (nums) (apply-cont cont (list-val (map num-val nums)))]
      [var-exp (var) (apply-cont cont (deref (apply-env env var)))]
      [diff-exp (exp1 exp2) (value-of/k exp1 env (cons (diff1-frame exp2 env) cont))]
      [if-exp (exp1 exp2 exp3) (value-of/k exp1 env (cons (if-test-frame exp2 exp3 env) cont))]
      [proc-exp (var body) (apply-cont cont (proc-val (procedure var body env)))]
      [call-exp (rator rand) (value-of/k rator env (cons (rator-frame rand env) cont))]
      [let-exp (var exp1 body) (value-of/k (call-exp (proc-exp var body) exp1) env cont)]
      [begin-exp (exp exps) (if (null? exps)
                                (value-of/k exp env cont)
                                (value-of/k (call-exp (proc-exp (fresh-identifier 'dummy)
                                                                (begin-exp (car exps) (cdr exps)))
                                                      exp)
                                            env
                                            cont))]
      [letrec-exp (p-names b-vars p-bodies letrec-body) (value-of/k letrec-body
                                                                    (extend-env-rec* p-names b-vars p-bodies env)
                                                                    cont)]
      [set-exp (id exp) (value-of/k exp env (cons (set-rhs-frame (apply-env env id)) cont))]
      [spawn-exp (exp) (value-of/k exp env (cons (spawn-frame) cont))]
      [yield-exp ()
                 (place-on-ready-queue! (lambda ()
                                          (apply-cont cont (num-val 99))))
                 (run-next-thread)]
      [mutex-exp () (apply-cont cont (mutex-val (new-mutex)))]
      [wait-exp (exp) (value-of/k exp env (cons (wait-frame) cont))]
      [signal-exp (exp) (value-of/k exp env (cons (signal-frame) cont))]
      [unop-exp (unop1 exp) (value-of/k exp env (cons (unop-arg-frame unop1) cont))])))

(define value-of-program
  (lambda (timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      [a-program (exp1) (value-of/k exp1 (empty-env) (list (end-main-thread-frame)))])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program 50 (scan&parse string))))

(provide num-val bool-val list-val run)
