#lang eopl

;; Exercise 1.28 [🟉🟉] (merge loi1 loi2), where loi1 and loi2 are lists of integers that are sorted in ascending order,
;; returns a sorted list of all the integers in loi1 and loi2.
;;
;;     > (merge '(1 4) '(1 2 8))
;;     (1 1 2 4 8)
;;     > (merge '(35 62 81 90 91) '(3 83 85 90))
;;     (3 35 62 81 83 85 90 90 91)

(define merge-helper
  (lambda (loi1 loi2)
    (if (null? loi1)
        loi2
        (let ([i1 (car loi1)]
              [i2 (car loi2)])
          (if (< i1 i2)
              (cons i1 (merge-helper (cdr loi1) loi2))
              (cons i2 (merge-helper (cdr loi2) loi1)))))))

(define merge
  (lambda (loi1 loi2)
    (if (null? loi1)
        loi2
        (merge-helper loi2 loi1))))

(provide merge)
