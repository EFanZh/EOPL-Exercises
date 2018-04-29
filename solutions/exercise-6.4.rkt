#lang eopl

;; Exercise 6.4 [★★] Rewrite each of the following procedures in continuation-passing style. For each procedure, do
;; this first using a data-structure representation of continuations, then with a procedural representation, and then
;; with the inlined procedural representation. Last, write the registerized version. For each of these four versions,
;; test to see that your implementation is tail-recursive by defining end-cont by
;;
;;     (apply-cont (end-cont) val)
;;     = (begin
;;         (eopl:printf "End of computation.~%")
;;         (eopl:printf "This sentence should appear only once.~%")
;;         val)
;;
;; as we did in chapter 5.
;;
;;     1. remove-first (section 1.2.3).
;;     2. list-sum (section 1.3).
;;     3. occurs-free? (section 1.2.4).
;;     4. subst (section 1.2.5).

(define the-end-cont
  (lambda (val)
    (begin (eopl:printf "End of computation.~%")
           (eopl:printf "This sentence should appear only once.~%")
           val)))

(define (uninitialized)
  'uninitialized)

;; remove-first: original.

(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

;; remove-first: data-structure representation.

(define remove-first-data-structure
  (let ()
    (define-datatype continuation continuation?
      [end-cont]
      [remove-first-cont [val1 symbol?]
                         [saved-cont continuation?]])

    (define apply-cont
      (lambda (cont val)
        (cases continuation cont
          [end-cont () (the-end-cont val)]
          [remove-first-cont (val1 saved-cont) (apply-cont saved-cont (cons val1 val))])))

    (define remove-first/k
      (lambda (s los cont)
        (if (null? los)
            (apply-cont cont '())
            (if (eqv? (car los) s)
                (apply-cont cont (cdr los))
                (remove-first/k s (cdr los) (remove-first-cont (car los) cont))))))

    (define remove-first
      (lambda (s los)
        (remove-first/k s los (end-cont))))

    remove-first))

;; remove-first: procedural representation.

(define remove-first-procedural
  (let ()
    (define end-cont
      (lambda ()
        the-end-cont))

    (define remove-first-cont
      (lambda (val1 saved-cont)
        (lambda (val)
          (saved-cont (cons val1 val)))))

    (define apply-cont
      (lambda (cont val)
        (cont val)))

    (define remove-first/k
      (lambda (s los cont)
        (if (null? los)
            (apply-cont cont '())
            (if (eqv? (car los) s)
                (apply-cont cont (cdr los))
                (remove-first/k s (cdr los) (remove-first-cont (car los) cont))))))

    (define remove-first
      (lambda (s los)
        (remove-first/k s los (end-cont))))

    remove-first))

;; remove-first: inlined procedural representation.

(define remove-first-inlined-procedural
  (let ()
    (define remove-first/k
      (lambda (s los cont)
        (if (null? los)
            (cont '())
            (if (eqv? (car los) s)
                (cont (cdr los))
                (let ([val1 (car los)])
                  (remove-first/k s
                                  (cdr los)
                                  (lambda (val)
                                    (cont (cons val1 val)))))))))

    (define remove-first
      (lambda (s los)
        (remove-first/k s los the-end-cont)))

    remove-first))

;; remove-first: registerized version.

(define remove-first-registerized
  (let ()
    (define the-s (uninitialized))
    (define the-los (uninitialized))
    (define the-cont (uninitialized))
    (define the-val (uninitialized))

    (define remove-first/k
      (lambda ()
        (if (null? the-los)
            (begin (set! the-val '())
                   (the-cont))
            (if (eqv? (car the-los) the-s)
                (begin (set! the-val (cdr the-los))
                       (the-cont))
                (let ([val1 (car the-los)]
                      [saved-cont the-cont])
                  (set! the-los (cdr the-los))
                  (set! the-cont (lambda ()
                                   (set! the-val (cons val1 the-val))
                                   (saved-cont)))
                  (remove-first/k))))))

    (define remove-first
      (lambda (s los)
        (set! the-s s)
        (set! the-los los)
        (set! the-cont (lambda ()
                         (the-end-cont the-val)))
        (remove-first/k)))

    remove-first))

;; list-sum: original.

(define list-sum
  (lambda (loi)
    (if (null? loi)
        0
        (+ (car loi)
           (list-sum (cdr loi))))))

;; list-sum: data-structure representation.

(define list-sum-data-structure
  (let ()
    (define-datatype continuation continuation?
      [end-cont]
      [list-sum-cont [val1 integer?]
                     [saved-cont continuation?]])

    (define apply-cont
      (lambda (cont val)
        (cases continuation cont
          [end-cont () (the-end-cont val)]
          [list-sum-cont (val1 saved-cont) (apply-cont saved-cont (+ val1 val))])))

    (define list-sum/k
      (lambda (loi cont)
        (if (null? loi)
            (apply-cont cont 0)
            (list-sum/k (cdr loi) (list-sum-cont (car loi) cont)))))

    (define list-sum
      (lambda (loi)
        (list-sum/k loi (end-cont))))

    list-sum))

;; list-sum: procedural representation.

(define list-sum-procedural
  (let ()
    (define end-cont
      (lambda ()
        the-end-cont))

    (define list-sum-cont
      (lambda (val1 saved-cont)
        (lambda (val)
          (saved-cont (+ val1 val)))))

    (define apply-cont
      (lambda (cont val)
        (cont val)))

    (define list-sum/k
      (lambda (loi cont)
        (if (null? loi)
            (apply-cont cont 0)
            (list-sum/k (cdr loi) (list-sum-cont (car loi) cont)))))

    (define list-sum
      (lambda (loi)
        (list-sum/k loi (end-cont))))

    list-sum))

;; list-sum: inlined procedural representation.

(define list-sum-inlined-procedural
  (let ()
    (define list-sum/k
      (lambda (loi cont)
        (if (null? loi)
            (cont 0)
            (let ([val1 (car loi)])
              (list-sum/k (cdr loi)
                          (lambda (val)
                            (cont (+ val1 val))))))))

    (define list-sum
      (lambda (loi)
        (list-sum/k loi the-end-cont)))

    list-sum))

;; list-sum: registerized version.

(define list-sum-registerized
  (let ()
    (define the-loi (uninitialized))
    (define the-cont (uninitialized))
    (define the-val (uninitialized))

    (define list-sum/k
      (lambda ()
        (if (null? the-loi)
            (begin (set! the-val 0)
                   (the-cont))
            (let ([val1 (car the-loi)]
                  [saved-cont the-cont])
              (set! the-loi (cdr the-loi))
              (set! the-cont (lambda ()
                               (set! the-val (+ val1 the-val))
                               (saved-cont)))
              (list-sum/k)))))

    (define list-sum
      (lambda (loi)
        (set! the-loi loi)
        (set! the-cont (lambda ()
                         (the-end-cont the-val)))
        (list-sum/k)))

    list-sum))

;; occurs-free?: original.

(define occurs-free?
  (lambda (var exp)
    (cond [(symbol? exp) (eqv? var exp)]
          [(eqv? (car exp) 'lambda) (and (not (eqv? var (car (cadr exp))))
                                         (occurs-free? var (caddr exp)))]
          [else (or (occurs-free? var (car exp))
                    (occurs-free? var (cadr exp)))])))

;; occurs-free?: data-structure representation.

(define occurs-free?-data-structure
  (let ()
    (define-datatype continuation continuation?
      [end-cont]
      [occurs-free?-cont [var symbol?]
                         [exp (lambda (x)
                                (or (symbol? x)
                                    (list? x)))]
                         [saved-cont continuation?]])

    (define apply-cont
      (lambda (cont val)
        (cases continuation cont
          [end-cont () (the-end-cont val)]
          [occurs-free?-cont (var exp saved-cont) (if val
                                                      (apply-cont saved-cont val)
                                                      (occurs-free?/k var (cadr exp) saved-cont))])))

    (define occurs-free?/k
      (lambda (var exp cont)
        (cond [(symbol? exp) (apply-cont cont (eqv? var exp))]
              [(eqv? (car exp) 'lambda) (let ([val1 (not (eqv? var (car (cadr exp))))])
                                          (if val1
                                              (occurs-free?/k var (caddr exp) cont)
                                              (apply-cont cont val1)))]
              [else (occurs-free?/k var (car exp) (occurs-free?-cont var exp cont))])))

    (define occurs-free?
      (lambda (var exp)
        (occurs-free?/k var exp (end-cont))))

    occurs-free?))

;; occurs-free?: procedural representation.

(define occurs-free?-procedural
  (let ()
    (define end-cont
      (lambda ()
        the-end-cont))

    (define occurs-free?-cont
      (lambda (var exp saved-cont)
        (lambda (val)
          (if val
              (apply-cont saved-cont val)
              (occurs-free?/k var (cadr exp) saved-cont)))))

    (define apply-cont
      (lambda (cont val)
        (cont val)))

    (define occurs-free?/k
      (lambda (var exp cont)
        (cond [(symbol? exp) (apply-cont cont (eqv? var exp))]
              [(eqv? (car exp) 'lambda) (let ([val1 (not (eqv? var (car (cadr exp))))])
                                          (if val1
                                              (occurs-free?/k var (caddr exp) cont)
                                              (apply-cont cont val1)))]
              [else (occurs-free?/k var (car exp) (occurs-free?-cont var exp cont))])))

    (define occurs-free?
      (lambda (var exp)
        (occurs-free?/k var exp (end-cont))))

    occurs-free?))

;; occurs-free?: inlined procedural representation.

(define occurs-free?-inlined-procedural
  (let ()
    (define occurs-free?/k
      (lambda (var exp cont)
        (cond [(symbol? exp) (cont (eqv? var exp))]
              [(eqv? (car exp) 'lambda) (let ([val1 (not (eqv? var (car (cadr exp))))])
                                          (if val1
                                              (occurs-free?/k var (caddr exp) cont)
                                              (cont val1)))]
              [else (occurs-free?/k var
                                    (car exp)
                                    (lambda (val)
                                      (if val
                                          (cont val)
                                          (occurs-free?/k var (cadr exp) cont))))])))

    (define occurs-free?
      (lambda (var exp)
        (occurs-free?/k var exp the-end-cont)))

    occurs-free?))

;; occurs-free?: registerized version.

(define occurs-free?-registerized
  (let ()
    (define the-var (uninitialized))
    (define the-exp (uninitialized))
    (define the-cont (uninitialized))
    (define the-val (uninitialized))

    (define occurs-free?/k
      (lambda ()
        (cond [(symbol? the-exp) (set! the-val (eqv? the-var the-exp))
                                 (the-cont)]
              [(eqv? (car the-exp) 'lambda) (let ([val1 (not (eqv? the-var (car (cadr the-exp))))])
                                              (if val1
                                                  (begin (set! the-exp (caddr the-exp))
                                                         (occurs-free?/k))
                                                  (begin (set! the-val val1)
                                                         (the-cont))))]
              [else (let ([exp the-exp]
                          [saved-cont the-cont])
                      (set! the-cont (lambda ()
                                       (if the-val
                                           (saved-cont)
                                           (begin (set! the-exp (cadr exp))
                                                  (occurs-free?/k))))))
                    (set! the-exp (car the-exp))
                    (occurs-free?/k)])))

    (define occurs-free?
      (lambda (var exp)
        (set! the-var var)
        (set! the-exp exp)
        (set! the-cont (lambda ()
                         (the-end-cont the-val)))
        (occurs-free?/k)))

    occurs-free?))

;; subst: original.

(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old)
            new
            sexp)
        (subst new old sexp))))

(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons (subst-in-s-exp new old (car slist))
              (subst new old (cdr slist))))))

;; subst: data-structure representation.

(define subst-data-structure
  (let ()
    (define sexp?
      (lambda (x)
        (or (symbol? x)
            (slist? x))))

    (define slist? (list-of sexp?))

    (define-datatype continuation continuation?
      [end-cont]
      [subst-in-s-exp-cont [new symbol?]
                           [old symbol?]
                           [slist slist?]
                           [saved-cont continuation?]]
      [subst-cont [val1 sexp?]
                  [saved-cont continuation?]])

    (define apply-cont
      (lambda (cont val)
        (cases continuation cont
          [end-cont () (the-end-cont val)]
          [subst-in-s-exp-cont (new old slist saved-cont) (subst/k new old (cdr slist) (subst-cont val saved-cont))]
          [subst-cont (val1 saved-cont) (apply-cont saved-cont (cons val1 val))])))

    (define subst-in-s-exp/k
      (lambda (new old sexp cont)
        (if (symbol? sexp)
            (if (eqv? sexp old)
                (apply-cont cont new)
                (apply-cont cont sexp))
            (subst/k new old sexp cont))))

    (define subst/k
      (lambda (new old slist cont)
        (if (null? slist)
            (apply-cont cont '())
            (subst-in-s-exp/k new old (car slist) (subst-in-s-exp-cont new old slist cont)))))

    (define subst
      (lambda (new old slist)
        (subst/k new old slist (end-cont))))

    subst))

;; subst: procedural representation.

(define subst-procedural
  (let ()
    (define end-cont
      (lambda ()
        the-end-cont))

    (define subst-in-s-exp-cont
      (lambda (new old slist saved-cont)
        (lambda (val)
          (subst/k new old (cdr slist) (subst-cont val saved-cont)))))


    (define subst-cont
      (lambda (val1 saved-cont)
        (lambda (val)
          (apply-cont saved-cont (cons val1 val)))))

    (define apply-cont
      (lambda (cont val)
        (cont val)))

    (define subst-in-s-exp/k
      (lambda (new old sexp cont)
        (if (symbol? sexp)
            (if (eqv? sexp old)
                (apply-cont cont new)
                (apply-cont cont sexp))
            (subst/k new old sexp cont))))

    (define subst/k
      (lambda (new old slist cont)
        (if (null? slist)
            (apply-cont cont '())
            (subst-in-s-exp/k new old (car slist) (subst-in-s-exp-cont new old slist cont)))))

    (define subst
      (lambda (new old slist)
        (subst/k new old slist (end-cont))))

    subst))

;; subst: inlined procedural representation.

(define subst-inlined-procedural
  (let ()
    (define subst-in-s-exp/k
      (lambda (new old sexp cont)
        (if (symbol? sexp)
            (if (eqv? sexp old)
                (cont new)
                (cont sexp))
            (subst/k new old sexp cont))))

    (define subst/k
      (lambda (new old slist cont)
        (if (null? slist)
            (cont '())
            (subst-in-s-exp/k new
                              old
                              (car slist)
                              (lambda (val1)
                                (subst/k new
                                         old
                                         (cdr slist)
                                         (lambda (val2)
                                           (cont (cons val1 val2)))))))))

    (define subst
      (lambda (new old slist)
        (subst/k new old slist the-end-cont)))

    subst))

;; subst: registerized version.

(define subst-registerized
  (let ()
    (define the-new (uninitialized))
    (define the-old (uninitialized))
    (define the-sexp (uninitialized))
    (define the-slist (uninitialized))
    (define the-cont (uninitialized))
    (define the-val (uninitialized))

    (define subst-in-s-exp/k
      (lambda ()
        (if (symbol? the-sexp)
            (if (eqv? the-sexp the-old)
                (begin (set! the-val the-new)
                       (the-cont))
                (begin (set! the-val the-sexp)
                       (the-cont)))
            (begin (set! the-slist the-sexp)
                   (subst/k)))))

    (define subst/k
      (lambda ()
        (if (null? the-slist)
            (begin (set! the-val '())
                   (the-cont))
            (begin (let ([slist the-slist]
                         [saved-cont the-cont])
                     (set! the-cont (lambda ()
                                      (set! the-slist (cdr slist))
                                      (let ([val1 the-val])
                                        (set! the-cont (lambda ()
                                                         (set! the-val (cons val1 the-val))
                                                         (saved-cont))))
                                      (subst/k))))
                   (set! the-sexp (car the-slist))
                   (subst-in-s-exp/k)))))

    (define subst
      (lambda (new old slist)
        (set! the-new new)
        (set! the-old old)
        (set! the-slist slist)
        (set! the-cont (lambda ()
                         (the-end-cont the-val)))
        (subst/k)))

    subst))

;; Interface.

(provide remove-first
         remove-first-data-structure
         remove-first-procedural
         remove-first-inlined-procedural
         remove-first-registerized
         list-sum
         list-sum-data-structure
         list-sum-procedural
         list-sum-inlined-procedural
         list-sum-registerized
         occurs-free?
         occurs-free?-data-structure
         occurs-free?-procedural
         occurs-free?-inlined-procedural
         occurs-free?-registerized
         subst
         subst-data-structure
         subst-procedural
         subst-inlined-procedural
         subst-registerized)
