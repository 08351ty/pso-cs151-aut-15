#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/uchicago151.rkt")

;; defines a point with x and y coordinates
(define-struct Point
  ([x : Integer]
   [y : Integer]))

;; defines a data set which is a list of points
(define-type Dataset (Listof Point))

;; defines a line with slope m and y coordinate b
(define-struct Line
  ([m : Real]
   [b : Real]))

(: num-points (-> Dataset Integer))
;; count the number of points in the dataset
(define (num-points s)
  (cond
    [(empty? s) 0]
    [(cons? s) (+ 1 (num-points (rest s)))]))

(: x-coords (-> Dataset (Listof Real)))
;; extract the x coordinates from a list of points
(define (x-coords s)
  (cond
    [(empty? s) empty]
    [(cons? s) (cons (Point-x (first s))
                     (x-coords (rest s)))]))
(check-expect (x-coords (list (Point 1 1) (Point 2 2))) (list 1 2))
(check-expect (x-coords (list (Point 0 2) (Point 0 6))) (list 0 0))
(check-expect (x-coords (list (Point 1 1)
                              (Point 2 2)
                              (Point 3 3)
                              (Point 4 4)
                              (Point 5 5))) (list 1 2 3 4 5))

(: y-coords (-> Dataset (Listof Real)))
;; extract the y coordinates from a list of points
(define (y-coords s)
  (cond
    [(empty? s) empty]
    [(cons? s) (cons (Point-y (first s))
                     (y-coords (rest s)))]))
(check-expect (y-coords (list (Point 1 1) (Point 2 2))) (list 1 2))
(check-expect (y-coords (list (Point 0 2) (Point 0 6))) (list 2 6))
(check-expect (y-coords (list (Point 1 1)
                              (Point 2 2)
                              (Point 3 3)
                              (Point 4 4)
                              (Point 5 5))) (list 1 2 3 4 5))

(: sum-list (-> (Listof Real) Real))
;;finds the sum of a list of reals
(define (sum-list list)
  (cond
    [(empty? list) 0]
    [else (+ (first list) (sum-list (rest list)))]))
(check-expect (sum-list (list 1 2 3 4 5)) 15)
(check-expect (sum-list (list 8 8 8 8 8 8 8 8 8 8)) 80)

(: product (-> Dataset (Listof Real)))
;;returns a list with the product of the x and y coordinates of each
;;point in initial dataset
(define (product s)
  (cond
    [(empty? s) empty]
    [(cons? s) (cons (* (Point-x (first s))
                        (Point-y (first s)))
                     (product (rest s)))]))
(check-expect (product (list (Point 1 1) (Point 2 2))) (list 1 4))
(check-expect (product (list (Point 6 6)
                             (Point 8 8)
                             (Point 12 12)
                             (Point 3 2))) (list 36 64 144 6))

(: x-coords-squared (-> Dataset (Listof Real)))
;; extract the x coordinates from a list of points and returns a list
;;with x^2 values
(define (x-coords-squared s)
  (cond
    [(empty? s) empty]
    [(cons? s) (cons (* (Point-x (first s))
                        (Point-x (first s)))
                     (x-coords-squared (rest s)))]))
(check-expect (x-coords-squared (list (Point 1 1)
                                      (Point 2 2))) (list 1 4))
(check-expect (x-coords-squared (list (Point 4 1)
                                      (Point 16 2)
                                      (Point 3 3)
                                      (Point 0 17))) (list 16 256 9 0))

(: slope (-> Dataset Real))
;; calculates slope of a given list of points
(define (slope data)
  (local
    {(define sumx (sum-list (x-coords data)))
     (define sumy (sum-list (y-coords data)))
     (define numpoints (num-points data))}
    (/ (- (* numpoints (sum-list (product data)))
          (* sumx sumy))
       (- (* numpoints (sum-list (x-coords-squared data)))
          (* sumx sumx)))))
(check-expect (slope (list (Point 0 0) (Point 1 3) (Point 2 6))) 3)
(check-expect (slope (list (Point 4 0) (Point 0 4))) -1)

(: intercept (-> Dataset Real))
;; calculates intercept of a given list of points
(define (intercept data)
  (local
    {(define sumx (sum-list (x-coords data)))
     (define sumy (sum-list (y-coords data)))
     (define numpoints (num-points data))}
    (/ (- (* sumy (sum-list (x-coords-squared data)))
          (* sumx (sum-list (product data))))
       (- (* numpoints (sum-list (x-coords-squared data)))
          (* sumx sumx)))))
(check-expect (intercept (list (Point 0 4) (Point 4 0))) 4)
(check-expect (intercept (list (Point 0 0) (Point 1 3) (Point 2 6))) 0)

(: linreg (-> Dataset Line))
;; calculates line from a data set of points
(define (linreg data)
  (Line (slope data) (intercept data)))
(check-expect (linreg (list (Point 0 4) (Point 4 0))) (Line -1 4))
(check-expect (linreg (list (Point 0 0) (Point 1 3) (Point 2 6))) (Line 3 0))

(test)
  
;; ADDED BY GRADER

"Running tests added by grader"
(require typed/test-engine/racket-tests)

(check-expect (slope (list (Point -1 -3) (Point 6 11))) 2)
(check-expect (slope (list (Point 1 10) (Point -6 10))) 0)
(check-within (slope (list (Point -5 6) (Point 10 2) (Point 1 3))) -0.254 0.001)

(check-within (intercept (list (Point 1 7) (Point -5 6) (Point 10 30))) 10.982 0.001)
(check-expect (intercept (list (Point 100 5) (Point -100 5))) 5)

(check-expect (linreg (list (Point 0 0) (Point 100 100))) (Line 1 0))
(check-expect (linreg (list (Point -10 5) (Point 10 15))) (Line 1/2 10))
(check-expect (linreg (list (Point -10 -10) (Point 10 10) (Point 40 40)))
              (Line 1 0))

(test)

;; === correctness ===

;; helpers       10/ 10
;; slope         10/ 10
;; intercept     10/ 10
;; linreg        10/ 10

;; === style ===

;; code layout                        8/  8
;; identifiers are well named         8/  8
;; program decomposition (helpers)    8/  8

;; contracts (type ascriptions)       8/  8
;; well-written purposes              8/  8

;; adequate tests                     8/  8

;; clarity (clear logic)              8/  8

;; svn used correctly                 4/  4

;; _total-score_                     100 / 100

;; graded by PRAMOD

