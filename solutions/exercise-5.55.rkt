#lang eopl

;; Exercise 5.55 [**] Add to the interpreter of exercise 5.53 an interthread communication facility, in which each
;; thread can send a value to another thread using its thread identifier. A thread can receive messages when it chooses,
;; blocking if no message has been sent to it.

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
    [expression ("send" "(" expression "," expression ")") send-exp]
    [expression ("receive" "(" ")") receive-exp]
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

(define remove-first
  (lambda (pred q)
    (let loop ([lst '()]
               [q q])
      (if (null? q)
          #f
          (let ([head (car q)]
                [tail (cdr q)])
            (if (pred head)
                (append (reverse lst) tail)
                (loop (cons head lst)
                      tail)))))))

;; Thread.

(define-datatype thread-context thread-context?
  [a-thread-context [id integer?]
                    [ref-to-messages reference?]
                    [message-mutex mutex?]])

(define thread-context->id
  (lambda (th-context)
    (cases thread-context th-context
      [a-thread-context (id ref-to-messages message-mutex) id])))

(define the-list-of-thread-contexts '())

(define fresh-thread-id
  (let ([last-thread-id -1])
    (lambda ()
      (set! last-thread-id (+ last-thread-id 1))
      last-thread-id)))

(define new-thread-context
  (lambda ()
    (let ([th-context (a-thread-context (fresh-thread-id) (newref (empty-queue)) (new-mutex))])
      (set! the-list-of-thread-contexts (cons th-context the-list-of-thread-contexts))
      th-context)))

(define remove-thread-context
  (lambda (th-context)
    (let ([result (remove-first (lambda (th-context1)
                                  (eq? th-context1 th-context))
                                the-list-of-thread-contexts)])
      (if result
          (set! the-list-of-thread-contexts result)
          (eopl:error 'remove-thread-context "failed to remove thread of id ~s" (thread-context->id th-context))))))

(define get-thread-context
  (lambda (id)
    (let loop ([th-contexts the-list-of-thread-contexts])
      (if (null? th-contexts)
          (eopl:error 'get-thread-context "thread not found for id ~s" id)
          (let ([th-context (car th-contexts)])
            (if (= (thread-context->id th-context) id)
                th-context
                (loop (cdr th-contexts))))))))

(define send-message
  (lambda (thread-id message th)
    (let ([th-context (get-thread-context thread-id)])
      (cases thread-context th-context
        [a-thread-context (id ref-to-messages message-mutex)
                          (setref! ref-to-messages (enqueue (deref ref-to-messages) message))
                          (signal-mutex message-mutex th)]))))

(define receive-message
  (lambda (cont th-context)
    (cases thread-context th-context
      [a-thread-context (id ref-to-messages message-mutex)
                        (letrec ([handle-message (lambda ()
                                                   (let ([messages (deref ref-to-messages)])
                                                     (if (null? messages)
                                                         (wait-for-mutex message-mutex handle-message)
                                                         (dequeue messages
                                                                  (lambda (message rest-messages)
                                                                    (setref! ref-to-messages rest-messages)
                                                                    (apply-cont cont message th-context))))))])
                          (wait-for-mutex message-mutex handle-message))])))

;; Scheduler.

(define the-ready-queue 'uninitialized)
(define the-final-answer 'uninitialized)
(define the-max-time-slice 'uninitialized)
(define the-time-remaining 'uninitialized)
(define the-current-thread-id 'uninitialized)

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
  [send1-cont [exp2 expression?]
              [env environment?]
              [saved-cont continuation?]]
  [send2-cont [threadid-val expval?]
              [saved-cont continuation?]]
  [unop-arg-cont [unop1 unop?]
                 [cont continuation?]])

;; Interpreter.

(define apply-unop
  (lambda (unop1 arg cont th-context)
    (cases unop unop1
      [zero?-unop () (apply-cont cont (bool-val (zero? (expval->num arg))) th-context)]
      [car-unop () (let ([lst (expval->list arg)])
                     (apply-cont cont (car lst) th-context))]
      [cdr-unop () (let ([lst (expval->list arg)])
                     (apply-cont cont (list-val (cdr lst)) th-context))]
      [null?-unop () (apply-cont cont (bool-val (null? (expval->list arg))) th-context)]
      [print-unop () (begin (eopl:printf "~a~%" (expval->num arg))
                            (apply-cont cont (num-val 1) th-context))])))

(define apply-procedure
  (lambda (proc1 arg cont th-context)
    (cases proc proc1
      [procedure (var body saved-env) (value-of/k body (extend-env var (newref arg) saved-env) cont th-context)])))

(define apply-cont
  (lambda (cont val th-context)
    (if (time-expired?)
        (begin (place-on-ready-queue! (lambda ()
                                        (apply-cont cont val th-context)))
               (run-next-thread))
        (begin (decrement-timer!)
               (cases continuation cont
                 [end-main-thread-cont ()
                                       (set-final-answer! val)
                                       (remove-thread-context th-context)
                                       (run-next-thread)]
                 [end-subthread-cont ()
                                     (remove-thread-context th-context)
                                     (run-next-thread)]
                 [diff1-cont (exp2 saved-env saved-cont) (value-of/k exp2
                                                                     saved-env
                                                                     (diff2-cont val saved-cont)
                                                                     th-context)]
                 [diff2-cont (val1 saved-cont) (let ([n1 (expval->num val1)]
                                                     [n2 (expval->num val)])
                                                 (apply-cont saved-cont (num-val (- n1 n2)) th-context))]
                 [if-test-cont (exp2 exp3 env cont) (if (expval->bool val)
                                                        (value-of/k exp2 env cont th-context)
                                                        (value-of/k exp3 env cont th-context))]
                 [rator-cont (rand saved-env saved-cont) (value-of/k rand
                                                                     saved-env
                                                                     (rand-cont val saved-cont)
                                                                     th-context)]
                 [rand-cont (val1 saved-cont) (let ([proc (expval->proc val1)])
                                                (apply-procedure proc val saved-cont th-context))]
                 [set-rhs-cont (loc cont)
                               (setref! loc val)
                               (apply-cont cont (num-val 26) th-context)]
                 [spawn-cont (saved-cont) (let* ([proc1 (expval->proc val)]
                                                 [th-context1 (new-thread-context)]
                                                 [thread-id-val (num-val (thread-context->id th-context1))])
                                            (place-on-ready-queue! (lambda ()
                                                                     (apply-procedure proc1
                                                                                      thread-id-val
                                                                                      (end-subthread-cont)
                                                                                      th-context1)))
                                            (apply-cont saved-cont thread-id-val th-context))]
                 [wait-cont (saved-cont) (wait-for-mutex (expval->mutex val)
                                                         (lambda ()
                                                           (apply-cont saved-cont (num-val 52) th-context)))]
                 [signal-cont (saved-cont) (signal-mutex (expval->mutex val)
                                                         (lambda ()
                                                           (apply-cont saved-cont (num-val 53) th-context)))]
                 [send1-cont (exp2 saved-env saved-cont) (value-of/k exp2
                                                                     saved-env
                                                                     (send2-cont val saved-cont)
                                                                     th-context)]
                 [send2-cont (thread-id-val saved-cont) (send-message (expval->num thread-id-val)
                                                                      val
                                                                      (lambda ()
                                                                        (apply-cont saved-cont
                                                                                    (num-val 678)
                                                                                    th-context)))]
                 [unop-arg-cont (unop1 cont) (apply-unop unop1 val cont th-context)])))))

(define value-of/k
  (lambda (exp env cont th-context)
    (cases expression exp
      [const-exp (num) (apply-cont cont (num-val num) th-context)]
      [const-list-exp (nums) (apply-cont cont (list-val (map num-val nums)) th-context)]
      [var-exp (var) (apply-cont cont (deref (apply-env env var)) th-context)]
      [diff-exp (exp1 exp2) (value-of/k exp1 env (diff1-cont exp2 env cont) th-context)]
      [if-exp (exp1 exp2 exp3) (value-of/k exp1 env (if-test-cont exp2 exp3 env cont) th-context)]
      [proc-exp (var body) (apply-cont cont (proc-val (procedure var body env)) th-context)]
      [call-exp (rator rand) (value-of/k rator env (rator-cont rand env cont) th-context)]
      [let-exp (var exp1 body) (value-of/k (call-exp (proc-exp var body) exp1) env cont th-context)]
      [begin-exp (exp exps) (if (null? exps)
                                (value-of/k exp env cont th-context)
                                (value-of/k (call-exp (proc-exp (fresh-identifier 'dummy)
                                                                (begin-exp (car exps) (cdr exps)))
                                                      exp)
                                            env
                                            cont
                                            th-context))]
      [letrec-exp (p-names b-vars p-bodies letrec-body) (value-of/k letrec-body
                                                                    (extend-env-rec* p-names b-vars p-bodies env)
                                                                    cont
                                                                    th-context)]
      [set-exp (id exp) (value-of/k exp env (set-rhs-cont (apply-env env id) cont) th-context)]
      [spawn-exp (exp) (value-of/k exp env (spawn-cont cont) th-context)]
      [yield-exp ()
                 (place-on-ready-queue! (lambda ()
                                          (apply-cont cont (num-val 99) th-context)))
                 (run-next-thread)]
      [mutex-exp () (apply-cont cont (mutex-val (new-mutex)) th-context)]
      [wait-exp (exp) (value-of/k exp env (wait-cont cont) th-context)]
      [signal-exp (exp) (value-of/k exp env (signal-cont cont) th-context)]
      [send-exp (exp1 exp2) (value-of/k exp1 env (send1-cont exp2 env cont) th-context)]
      [receive-exp () (receive-message cont th-context)]
      [unop-exp (unop1 exp) (value-of/k exp env (unop-arg-cont unop1 cont) th-context)])))

(define value-of-program
  (lambda (timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      [a-program (exp1) (let ([th-context (new-thread-context)])
                          (value-of/k exp1 (empty-env) (end-main-thread-cont) th-context))])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program 50 (scan&parse string))))

(provide num-val bool-val list-val run)
