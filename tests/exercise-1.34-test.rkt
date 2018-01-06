#lang racket

(require rackunit)
(require "../solutions/exercise-1.34.rkt")

(check-equal? (path 17 '(14 (7 () (12 () ()))
                            (26 (20 (17 () ())
                                    ())
                                (31 () ()))))
              '(right left left))

(check-equal? (path 7 '(7 () ())) '())

(check-equal? (path 14 '(14 (7 () (12 () ()))
                            (26 (20 (17 () ())
                                    ())
                                (31 () ()))))
              '())

(check-equal? (path 7 '(14 (7 () (12 () ()))
                           (26 (20 (17 () ())
                                   ())
                               (31 () ()))))
              '(left))

(check-equal? (path 12 '(14 (7 () (12 () ()))
                            (26 (20 (17 () ())
                                    ())
                                (31 () ()))))
              '(left right))

(check-equal? (path 31 '(14 (7 () (12 () ()))
                            (26 (20 (17 () ())
                                    ())
                                (31 () ()))))
              '(right right))

