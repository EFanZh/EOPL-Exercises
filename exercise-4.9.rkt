#lang eopl

;; Exercise 4.9 [★] Implement the store in constant time by representing it as a Scheme vector. What is lost by using
;; this representation?

(define (empty-store)
  (vector))

(define the-store 'uninitialized)

(define (get-store)
  the-store)

(define (initialize-store!)
  (set! the-store (empty-store)))

(define (reference? v)
  (and (integer? v)
       (not (negative? v))))

(define (extend-store store val)
  (let* ([store-size (vector-length store)]
         [new-store (make-vector (+ store-size 1))])
    (let loop ([i 0])
      (if (< i store-size)
          (let ([val (vector-ref store i)])
            (vector-set! new-store i val)
            (loop (+ i 1)))
          (vector-set! new-store i val)))
    (cons new-store store-size)))

(define (newref val)
  (let* ([new-store-info (extend-store the-store val)]
         [new-store (car new-store-info)]
         [new-ref (cdr new-store-info)])
    (set! the-store new-store)
    new-ref))

(define (deref ref)
  (vector-ref the-store ref))

(define (report-invalid-reference ref store)
  (eopl:error 'setref
              "illegal reference ~s in store ~s"
              ref
              store))

(define (setref! ref val)
  (if (and (reference? ref)
           (< ref (vector-length the-store)))
      (vector-set! the-store ref val)
      (report-invalid-reference ref the-store)))

(provide initialize-store! newref deref setref!)
