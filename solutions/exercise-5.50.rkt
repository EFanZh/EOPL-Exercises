#lang eopl

;; Exercise 5.50 [★★] Registerize the interpreter of this section. What is the set of mutually tail-recursive procedures
;; that must be registerized?

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
  (lambda ()
    (cases mutex the-mutex
      [a-mutex (ref-to-closed? ref-to-wait-queue)
               (cond [(deref ref-to-closed?) (setref! ref-to-wait-queue (enqueue (deref ref-to-wait-queue) the-thread))
                                             (run-next-thread)]
                     [else (setref! ref-to-closed? #t)
                           (the-thread)])])))

(define signal-mutex
  (lambda ()
    (cases mutex the-mutex
      [a-mutex (ref-to-closed? ref-to-wait-queue) (let ([closed? (deref ref-to-closed?)]
                                                        [wait-queue (deref ref-to-wait-queue)])
                                                    (when closed?
                                                      (if (empty? wait-queue)
                                                          (setref! ref-to-closed? #f)
                                                          (dequeue wait-queue
                                                                   (lambda (first-waiting-th other-waiting-ths)
                                                                     (place-on-ready-queue! first-waiting-th)
                                                                     (setref! ref-to-wait-queue other-waiting-ths)))))
                                                    (the-thread))])))

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

(define-datatype continuation continuation?
  [end-main-thread-cont]
  [end-subthread-cont]
  [diff1-cont [exp2 expression?]
              [env environment?]
              [cont continuation?]]
  [diff2-cont [val1 expval?]
              [cont continuation?]]
  [if-test-cont [exp2 expression?]
                [exp3 expression?]
                [env environment?]
                [cont continuation?]]
  [rator-cont [rand expression?]
              [env environment?]
              [cont continuation?]]
  [rand-cont [val1 expval?]
             [cont continuation?]]
  [set-rhs-cont [loc reference?]
                [cont continuation?]]
  [spawn-cont [saved-cont continuation?]]
  [wait-cont [saved-cont continuation?]]
  [signal-cont [saved-cont continuation?]]
  [unop-arg-cont [unop1 unop?]
                 [cont continuation?]])

;; Interpreter.

(define the-exp 'uninitialized)
(define the-env 'uninitialized)
(define the-cont 'uninitialized)
(define the-val 'uninitialized)
(define the-proc 'uninitialized)
(define the-arg 'uninitialized)
(define the-unop 'uninitialized)
(define the-mutex 'uninitialized)
(define the-thread 'uninitialized)

(define apply-unop
  (lambda ()
    (cases unop the-unop
      [zero?-unop ()
                  (set! the-val (bool-val (zero? (expval->num the-arg))))
                  (apply-cont)]
      [car-unop () (let ([lst (expval->list the-arg)])
                     (set! the-val (car lst))
                     (apply-cont))]
      [cdr-unop () (let ([lst (expval->list the-arg)])
                     (set! the-val (list-val (cdr lst)))
                     (apply-cont))]
      [null?-unop ()
                  (set! the-val (bool-val (null? (expval->list the-arg))))
                  (apply-cont)]
      [print-unop () (begin (eopl:printf "~a~%" (expval->num the-arg))
                            (set! the-val (num-val 1))
                            (apply-cont))])))

(define apply-procedure
  (lambda ()
    (cases proc the-proc
      [procedure (var body saved-env)
                 (set! the-exp body)
                 (set! the-env (extend-env var (newref the-arg) saved-env))
                 (value-of/k)])))

(define apply-cont
  (lambda ()
    (if (time-expired?)
        (begin (place-on-ready-queue! (let ([cont the-cont]
                                            [val the-val])
                                        (lambda ()
                                          (set! the-cont cont)
                                          (set! the-val val)
                                          (apply-cont))))
               (run-next-thread))
        (begin (decrement-timer!)
               (cases continuation the-cont
                 [end-main-thread-cont ()
                                       (set-final-answer! the-val)
                                       (run-next-thread)]
                 [end-subthread-cont () (run-next-thread)]
                 [diff1-cont (exp2 saved-env saved-cont)
                             (set! the-exp exp2)
                             (set! the-env saved-env)
                             (set! the-cont (diff2-cont the-val saved-cont))
                             (value-of/k)]
                 [diff2-cont (val1 saved-cont) (let ([n1 (expval->num val1)]
                                                     [n2 (expval->num the-val)])
                                                 (set! the-cont saved-cont)
                                                 (set! the-val (num-val (- n1 n2)))
                                                 (apply-cont))]
                 [if-test-cont (exp2 exp3 env cont)
                               (set! the-exp
                                     (if (expval->bool the-val)
                                         exp2
                                         exp3))
                               (set! the-env env)
                               (set! the-cont cont)
                               (value-of/k)]
                 [rator-cont (rand saved-env saved-cont)
                             (set! the-exp rand)
                             (set! the-env saved-env)
                             (set! the-cont (rand-cont the-val saved-cont))
                             (value-of/k)]
                 [rand-cont (val1 saved-cont) (let ([proc (expval->proc val1)])
                                                (set! the-proc proc)
                                                (set! the-arg the-val)
                                                (set! the-cont saved-cont)
                                                (apply-procedure))]
                 [set-rhs-cont (loc cont)
                               (setref! loc the-val)
                               (set! the-cont cont)
                               (set! the-val (num-val 26))
                               (apply-cont)]
                 [spawn-cont (saved-cont) (let ([proc1 (expval->proc the-val)])
                                            (place-on-ready-queue! (lambda ()
                                                                     (set! the-proc proc1)
                                                                     (set! the-arg (num-val 28))
                                                                     (set! the-cont (end-subthread-cont))
                                                                     (apply-procedure)))
                                            (set! the-cont saved-cont)
                                            (set! the-val (num-val 73))
                                            (apply-cont))]
                 [wait-cont (saved-cont)
                            (set! the-mutex (expval->mutex the-val))
                            (set! the-thread (lambda ()
                                               (set! the-cont saved-cont)
                                               (set! the-val (num-val 52))
                                               (apply-cont)))
                            (wait-for-mutex)]
                 [signal-cont (saved-cont)
                              (set! the-mutex (expval->mutex the-val))
                              (set! the-thread (lambda ()
                                                 (set! the-cont saved-cont)
                                                 (set! the-val (num-val 53))
                                                 (apply-cont)))
                              (signal-mutex)]
                 [unop-arg-cont (unop1 cont)
                                (set! the-unop unop1)
                                (set! the-arg the-val)
                                (set! the-cont cont)
                                (apply-unop)])))))

(define value-of/k
  (lambda ()
    (cases expression the-exp
      [const-exp (num)
                 (set! the-val (num-val num))
                 (apply-cont)]
      [const-list-exp (nums)
                      (set! the-val (list-val (map num-val nums)))
                      (apply-cont)]
      [var-exp (var)
               (set! the-val (deref (apply-env the-env var)))
               (apply-cont)]
      [diff-exp (exp1 exp2)
                (set! the-exp exp1)
                (set! the-cont (diff1-cont exp2 the-env the-cont))
                (value-of/k)]
      [if-exp (exp1 exp2 exp3)
              (set! the-exp exp1)
              (set! the-cont (if-test-cont exp2 exp3 the-env the-cont))
              (value-of/k)]
      [proc-exp (var body)
                (set! the-val (proc-val (procedure var body the-env)))
                (apply-cont)]
      [call-exp (rator rand)
                (set! the-exp rator)
                (set! the-cont (rator-cont rand the-env the-cont))
                (value-of/k)]
      [let-exp (var exp1 body)
               (set! the-exp (call-exp (proc-exp var body) exp1))
               (value-of/k)]
      [begin-exp (exp exps)
                 (set! the-exp
                       (if (null? exps)
                           exp
                           (call-exp (proc-exp (fresh-identifier 'dummy) (begin-exp (car exps) (cdr exps))) exp)))
                 (value-of/k)]
      [letrec-exp (p-names b-vars p-bodies letrec-body)
                  (set! the-exp letrec-body)
                  (set! the-env (extend-env-rec* p-names b-vars p-bodies the-env))
                  (value-of/k)]
      [set-exp (id exp)
               (set! the-exp exp)
               (set! the-cont (set-rhs-cont (apply-env the-env id) the-cont))
               (value-of/k)]
      [spawn-exp (exp)
                 (set! the-exp exp)
                 (set! the-cont (spawn-cont the-cont))
                 (value-of/k)]
      [yield-exp ()
                 (place-on-ready-queue! (let ([cont the-cont])
                                          (lambda ()
                                            (set! the-cont cont)
                                            (set! the-val (num-val 99))
                                            (apply-cont))))
                 (run-next-thread)]
      [mutex-exp ()
                 (set! the-val (mutex-val (new-mutex)))
                 (apply-cont)]
      [wait-exp (exp)
                (set! the-exp exp)
                (set! the-cont (wait-cont the-cont))
                (value-of/k)]
      [signal-exp (exp)
                  (set! the-exp exp)
                  (set! the-cont (signal-cont the-cont))
                  (value-of/k)]
      [unop-exp (unop1 exp)
                (set! the-exp exp)
                (set! the-cont (unop-arg-cont unop1 the-cont))
                (value-of/k)])))

(define value-of-program
  (lambda (timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      [a-program (exp1)
                 (set! the-exp exp1)
                 (set! the-env (empty-env))
                 (set! the-cont (end-main-thread-cont))
                 (value-of/k)])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program 50 (scan&parse string))))

(provide num-val bool-val list-val run)
