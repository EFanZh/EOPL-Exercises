#lang eopl

;; Exercise 2.1 [🟉] Implement the four required operations for bigits. Then use your implementation to calculate the
;; factorial of 10. How does the execution time vary as this argument changes? How does the execution time vary as the
;; base changes? Explain why.

(define *base* 10)
(define *base-sub-1* (- *base* 1))

(define zero
  (lambda ()
    '()))

(define is-zero? null?)

(define successor
  (lambda (n)
    (if (null? n)
        '(1)
        (let ([lowest-digit (car n)])
          (if (= lowest-digit *base-sub-1*)
              (cons 0 (successor (cdr n)))
              (cons (+ lowest-digit 1) (cdr n)))))))

(define predecessor
  (lambda (n)
    (let ([lowest-digit (car n)]
          [rest-digits (cdr n)])
      (if (= lowest-digit 0)
          (cons *base-sub-1* (predecessor rest-digits))
          (if (and (= lowest-digit 1) (null? rest-digits))
              '()
              (cons (- lowest-digit 1) (cdr n)))))))

(define plus
  (lambda (m n)
    (if (is-zero? n)
        m
        (plus (successor m) (predecessor n)))))

(define multiply-helper
  (lambda (base m n)
    (if (is-zero? n)
        base
        (multiply-helper (plus base m) m (predecessor n)))))

(define multiply
  (lambda (m n)
    (multiply-helper (zero) m n)))

(define factorial-helper
  (lambda (base n)
    (if (is-zero? n)
        base
        (factorial-helper (multiply base n) (predecessor n)))))

(define factorial
  (lambda (n)
    (factorial-helper (successor (zero)) n)))

(provide zero is-zero? successor predecessor factorial)
