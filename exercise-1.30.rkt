#lang eopl

;; Exercise 1.30 [★★] (sort/predicate pred loi) returns a list of elements sorted by the predicate.
;;
;;     > (sort/predicate < '(8 2 5 2 3))
;;     (2 2 3 5 8)
;;     > (sort/predicate > '(8 2 5 2 3))
;;     (8 5 3 2 2)

(define get-run
  (lambda (pred loi)
    (let ([head1 (car loi)]
          [tail1 (cdr loi)])
      (if (null? tail1)
          (cons loi '())
          (let ([head2 (car tail1)])
            (if (pred head2 head1)
                (cons (list head1) tail1)
                (let ([tail-run (get-run pred tail1)])
                  (cons (cons head1 (car tail-run)) (cdr tail-run)))))))))

(define merge
  (lambda (pred run1 run2)
    (let ([head1 (car run1)]
          [head2 (car run2)])
      (if (pred head2 head1)
          (let ([tail2 (cdr run2)])
            (if (null? tail2)
                (cons head2 run1)
                (cons head2 (merge pred run1 tail2))))
          (let ([tail1 (cdr run1)])
            (if (null? tail1)
                (cons head1 run2)
                (cons head1 (merge pred tail1 run2))))))))

(define collapse-all
  (lambda (pred stack run)
    (if (null? stack)
        run
        (collapse-all pred (cdr stack) (merge pred (cdar stack) run)))))

(define collapse
  (lambda (pred stack level run)
    (if (null? stack)
        (list (cons level run))
        (let ([top (car stack)])
          (if (= (car top) level)
              (collapse pred (cdr stack) (+ level 1) (merge pred (cdr top) run))
              (cons (cons level run) stack))))))

(define sort-helper
  (lambda (pred stack loi)
    (let* ([run-and-tail (get-run pred loi)]
           [run (car run-and-tail)]
           [tail (cdr run-and-tail)])
      (if (null? tail)
          (collapse-all pred stack run)
          (sort-helper pred (collapse pred stack 0 run) tail)))))

(define sort/predicate
  (lambda (pred loi)
    (if (null? loi)
        '()
        (sort-helper pred '() loi))))

(provide sort/predicate)
