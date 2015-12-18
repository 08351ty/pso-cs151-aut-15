#lang typed/racket
(require typed/test-engine/racket-tests)
(require typed/2htdp/image)
(require "../include/uchicago151.rkt")

(: int-list-sum (-> (Listof Integer) Integer))
;;Computes the sum of a list of integers
(define (int-list-sum list)
  (cond
    [(empty? list) 0]
    [(cons? list) (+ (first list) (int-list-sum (rest list)))]))
(check-expect (int-list-sum (list 1 2 3 4 5)) 15)

(: int-list-product (-> (Listof Integer) Integer))
;;Compute the product of a list of integers
(define (int-list-product list)
  (cond
    [(cons? list) (* (first list) (int-list-product (rest list)))]
    [else 1]))
(check-expect (int-list-product (list 1 2 3 4 5)) 120)

(: evens (-> (Listof Integer) (Listof Integer)))
;;Build a list of only the even numbers in the input list
(define (evens list)
  (cond
    [(cons? list) (if (= (modulo (first list) 2) 0)
                      (cons (first list) (evens (rest list)))
                      (evens (rest list)))]
    [else empty]))
(check-expect (evens (list 1 2 3 4)) (list 2 4))

(: red-circles (-> (Listof Integer) (Listof Image)))
;;Produce a list of solid red circles of given radii
(define (red-circles list)
  (cond
    [(cons? list) (if (< 0 (first list))
                      (cons (circle (first list) "solid" "red") (red-circles (rest list)))
                      (cons empty-image (red-circles (rest list))))]
    [else empty]))
(check-expect (red-circles (list 5 10))
              (list (circle 5 "solid" "red")
                    (circle 10 "solid" "red")))

(: int-size (-> Integer Integer Integer))
;;Determines how big the size of the integer is in terms of 10^x
(define (int-size i p)
  (cond
    [(> i 0) (if (and (<= 1 (/ i p) 9))
                 p
                 (int-size i (* p 10)))]
    [else (error "entered a negative number")]))
(check-expect (int-size 4286 1) 1000)
  
(: int-to-list (-> Integer (Listof Integer)))
;;Produces a list containing the digits of the integer
(define (int-to-list i)
  (cond
    [(> i 0) (cons (quotient i (int-size i 1))
                    (int-to-list (- i (* (int-size i 1) (quotient i (int-size i 1))))))]
    [else empty]))
(check-expect (int-to-list 4286) (list 4 2 8 6))

(: explode-digits (-> Integer (Listof Integer)))
;;Consumes an integer and produces a list of the digits in that integer in reverse
(define (explode-digits i)
  (reverse (int-to-list i)))

;; grader comment: you're not handling 0 correctly --> you're just ignoring it
;; e.g. (explode-digits 0) = (list 0) and (explode-digits 10) = (list 0 1)
(check-expect (explode-digits 4286) (list 6 8 2 4))

(: implode-digits (-> (Listof Integer) Integer Integer))
;;Consumes a list of digits and composes them together into the corresponding number in reverse order
(define (implode-digits list int)
  (cond
    [(empty? list) 0]
    [(cons? list) (+ (* (first list) int)
                     (implode-digits (rest list)
                               (* int 10)))]))

;; if you're going to change the type signature you need to use this as a helper
;; you can't expect the user to know to pass in 1 for int
;; also you're not handling negative values or values > 9
(check-expect (implode-digits (list 4 2 8 6) 1) 6824)

;;Defines the type of coin in value from Dollar to Penny
(define-type Coin (U 'Dollar 'Quarter 'Dime 'Nickel 'Penny))

(: num-quarters (-> (Listof Coin) Integer))
;;Finds the number of quarters in a list of coins
(define (num-quarters list)
  (cond
    [(cons? list) (if (symbol=? (first list) 'Quarter)
                      (+ 1 (num-quarters (rest list)))
                      (num-quarters (rest list)))]
    [else 0]))
(check-expect (num-quarters (list 'Quarter 'Dollar 'Quarter 'Quarter 'Dime 'Nickel)) 3)
(check-expect (num-quarters (list 'Quarter 'Quarter 'Quarter 'Quarter 'Quarter 'Quarter)) 6)

(: list-size (-> (Listof Coin) Integer))
;;Finds the size of the list
(define (list-size list)
  (cond
    [(empty? list) 0]
    [(cons? list) (+ 1 (list-size (rest list)))]))
(check-expect (list-size (list 'Quarter 'Dollar 'Quarter)) 3)

(: num-dimes (-> (Listof Coin) Integer))
;;Finds the number of dimes in a list of coins
(define (num-dimes list)
  (cond
    [(cons? list) (if (symbol=? (first list) 'Dime)
                      (+ 1 (num-dimes (rest list)))
                      (num-dimes (rest list)))]
    [else 0]))
(check-expect (num-dimes (list 'Quarter 'Dollar 'Quarter 'Quarter 'Dime 'Nickel)) 1)
(check-expect (num-dimes (list 'Quarter 'Quarter 'Quarter 'Quarter 'Quarter 'Quarter)) 0)

(: pct-dimes (-> (Listof Coin) Exact-Rational))
;;Returns the percentage of dimes in the list
(define (pct-dimes list)
  (* (/ (num-dimes list) (list-size list)) 100))

;; grader comment: what about when you're given an empty list?
;; you should throw an error b/c you're trying t odivide by 0

(check-expect (pct-dimes (list 'Quarter 'Dime)) 50)
(check-expect (pct-dimes (list 'Quarter 'Dime 'Dime 'Dime)) 75)
(check-expect (pct-dimes (list 'Quarter 'Nickel)) 0)

(: contains-dollar? (-> (Listof Coin) Boolean))
;;Returns true if there are one or more dollar coins in the list
(define (contains-dollar? list)
  (cond
    [(cons? list) (if (symbol=? (first list) 'Dollar) #t (contains-dollar? (rest list)))]
    [else #f]))
(check-expect (contains-dollar? (list 'Quarter 'Dime)) #f)
(check-expect (contains-dollar? (list 'Quarter 'Dollar)) #t)

(: coin-total (-> (Listof Coin) Integer))
;;Returns the total value of the list of coins in pennies
(define (coin-total list)
  (cond
    [(cons? list) (cond
                    [(symbol=? (first list) 'Dollar) (+ 100 (coin-total (rest list)))]
                    [(symbol=? (first list) 'Quarter) (+ 25 (coin-total (rest list)))]
                    [(symbol=? (first list) 'Dime) (+ 10 (coin-total (rest list)))]
                    [(symbol=? (first list) 'Nickel) (+ 5 (coin-total (rest list)))]
                    [(symbol=? (first list) 'Penny) (+ 1 (coin-total (rest list)))])]
    [else 0]))
(check-expect (coin-total (list 'Quarter 'Dime)) 35)
(check-expect (coin-total (list 'Dollar 'Dollar 'Dollar 'Dollar 'Dime 'Dime)) 420)

(: change (-> Integer (Listof Coin)))
;;Takes in an integer and returns a list of coins adding up to that number of cents
(define (change i)
  (cond
    [(>= i 100) (cons 'Dollar (change (- i 100)))]
    [(>= i 25) (cons 'Quarter (change (- i 25)))]
    [(>= i 10) (cons 'Dime (change (- i 10)))]
    [(>= i 5) (cons 'Nickel (change (- i 5)))]
    [(>= i 1) (cons 'Penny (change (- i 1)))]
    [else empty]))
(check-expect (change 123) (list 'Dollar 'Dime 'Dime 'Penny 'Penny 'Penny))
(check-expect (change 75) (list 'Quarter 'Quarter 'Quarter))

(: show-coins (-> (Listof Coin) Image))
;;Produces an image of the given coins in red circles according to their size
;;Size: Dollar > Quarter > Nickel > Dime > Penny
(define (show-coins list)
  (cond
    [(cons? list) (cond
                    [(symbol=? (first list) 'Dollar)
                     (beside (circle 50 "solid" "red")
                             (show-coins (rest list)))]
                    [(symbol=? (first list) 'Quarter)
                     (beside (circle 40 "solid" "red")
                             (show-coins (rest list)))]
                    [(symbol=? (first list) 'Dime)
                     (beside (circle 20 "solid" "red")
                             (show-coins (rest list)))]
                    [(symbol=? (first list) 'Nickel)
                     (beside (circle 30 "solid" "red")
                             (show-coins (rest list)))]
                    [(symbol=? (first list) 'Penny)
                     (beside (circle 10 "solid" "red")
                             (show-coins (rest list)))]
                    [else empty-image])]
    [else empty-image]))
(check-expect (show-coins (list 'Dollar 'Penny)) (beside (circle 50 "solid" "red")
                                                         (circle 10 "solid" "red")))

;;Defines a term of a polynomial with coefficient and exponent
(define-struct Term
  ([coeff : Real]
   [exp   : Integer]))

;;Defines type polynomial that is a list of terms
(define-type Polynomial (Listof Term))

;;Appends the term as a string in form ax^b
(: term->string (-> Term String))
(define (term->string term)
  (string-append (number->string (Term-coeff term))
                 "x^" (number->string (Term-exp term))))
(check-expect (term->string (Term 3 2)) "3x^2")

;;Appends the polynomial as a string
(: polynomial->string (-> Polynomial String))
(define (polynomial->string poly)
  (cond
    [(cons? poly) (string-append
                   (term->string (first poly))
                   (if (cons? (rest poly))
                       (string-append " + " (polynomial->string (rest poly)))
                       ""))]
    [else ""]))
(check-expect (polynomial->string (list (Term 3 2) (Term 2 1))) "3x^2 + 2x^1")
(check-expect (polynomial->string (list (Term 4 7) (Term 3 2) (Term 2 1))) "4x^7 + 3x^2 + 2x^1")

;;Finds the value of the polynomial at a specific x
(: value-at (-> Polynomial Real Real))
(define (value-at poly x)
  (cond
    [(cons? poly) (+ (* (Term-coeff (first poly)) (expt x (Term-exp (first poly)))) (value-at (rest poly) x))]
    [else 0]))
(check-expect (value-at (list (Term 3 2) (Term 2 1)) 4) 56)
(check-expect (value-at (list (Term 4 7) (Term 3 2) (Term 2 1)) 1) 9)

;;Finds the derivative of the polynomial
(: derivative (-> Polynomial Polynomial))
(define (derivative poly)
  (cond
    [(cons? poly) (cons (Term (* (Term-coeff (first poly))
                                 (Term-exp (first poly)))
                              (- (Term-exp (first poly)) 1))
                        (derivative (rest poly)))]
    [else empty]))

;; grader comment: what happens when you take the derivative of a constant?
;; it should be 0, not <whatever> ^ -1

(check-expect (derivative (list (Term 3 3) (Term 2 2))) (list (Term 9 2) (Term 4 1)))

;;Finds the antiderivative of the polynomial
(: antiderivative (-> Polynomial Polynomial))
(define (antiderivative poly)
  (cond
    [(cons? poly) (cons (Term (/ (Term-coeff (first poly))
                                        (+ (Term-exp (first poly)) 1))
                              (+ (Term-exp (first poly)) 1))
                        (antiderivative (rest poly)))]
    [else empty]))
(check-expect (antiderivative (list (Term 4 3) (Term 3 2))) (list (Term 1 4) (Term 1 3)))

;;Creates a list of integers of exponents from a polynomial type
(: poly-exp (-> Polynomial (Listof Integer)))
(define (poly-exp poly)
  (cond
    [(cons? poly) (cons (Term-exp (first poly)) (poly-exp (rest poly)))]
    [else empty]))
(check-expect (poly-exp (list (Term 3 2) (Term 5 5) (Term 2 1))) (list 2 5 1))                                            

;;Checks whether a given polynomial is strictly descending in power
(: sorted-desc? (-> Polynomial Boolean))
(define (sorted-desc? poly)
  (cond
    [(and (cons? poly) (cons? (rest poly))) (if (> (first (poly-exp poly)) (first (poly-exp (rest poly))))
      #t #f)]
    [else #f]))
(check-expect (sorted-desc? (list (Term 8 9) (Term 7 7) (Term 3 2))) #t)
(check-expect (sorted-desc? (list (Term 2 8) (Term 2 9) (Term 3 2))) #f)
(check-expect (sorted-desc? (list (Term 3 2) (Term 2 1))) #t)
(check-expect (sorted-desc? (list (Term 4 2) (Term 2 1) (Term 1 0))) #t)

;;Adds two polynomials
(: polynomial-add (-> Polynomial Polynomial Polynomial))
(define (polynomial-add poly1 poly2)
  (cond
    [(and (cons? poly1) (cons? poly2))
     (cond
       [(= (Term-exp (first poly1)) (Term-exp (first poly2)))
        (cons (Term (+ (Term-coeff (first poly1)) (Term-coeff (first poly2)))
                    (Term-exp (first poly1)))
              (polynomial-add (rest poly1) (rest poly2)))]
       [(> (Term-exp (first poly1)) (Term-exp (first poly2)))
        (cons (Term (Term-coeff (first poly1)) (Term-exp (first poly1)))
              (polynomial-add (rest poly1) (rest poly2)))] ;; *****
       [(< (Term-exp (first poly1)) (Term-exp (first poly2)))
        (cons (Term (Term-coeff (first poly2)) (Term-exp (first poly2)))
              (polynomial-add (rest poly1) (rest poly2)))] ;; ****
       [else empty])]
    [(and (cons? poly1) (empty? poly2)) poly1]
    [(and (empty? poly1) (cons? poly2)) poly2]
    [else empty]))

;; grader comment: first, you probably want to verify that both polynomials are in descending order
;; because your function works under that assumptions
;; also at the points where there are "****", you made a small mistake where you call
;; (rest poly2) or (rest poly1), you should call (first poly2), since you didn't do
;; anything to the "first" of those values
  
(check-expect (polynomial-add (list (Term 3 2) (Term 2 1)) (list (Term 4 2) (Term 2 1) (Term 1 0)))
              (list (Term 7 2) (Term 4 1) (Term 1 0)))

;; Graders Tests

;; Problem 1
(check-expect (int-list-sum (list 1 2 3 4)) 10)
(check-expect (int-list-sum empty) 0)
(check-expect (int-list-sum (list -1 -2 -3 -4)) -10)
(check-expect (int-list-sum (list -1 -2 3 -4 7)) 3)
(check-expect (int-list-sum (list -279 825 392 -4200 729)) -2533)
(check-expect (int-list-sum empty) 0)

(check-expect (int-list-product (list 1 2 3 4)) 24)
(check-expect (int-list-product (list -1 -2 -3 -4)) 24)
(check-expect (int-list-product (list -1 -2 3 -4 7)) -168)
(check-expect (int-list-product (list -1 -2 3 -4 7 0)) 0)
(check-expect (int-list-product (list 3 -4 7)) -84)
(check-expect (int-list-product (list -279 825 0 392 -4200 729)) 0)
(check-expect (int-list-product empty) 1)

(check-expect (evens (list -279 825 0 392 -4200 729)) (list 0 392 -4200))
(check-expect (evens (list 2 3 4 4 5)) (list 2 4 4))
(check-expect (evens (list 0)) (list 0))
(check-expect (evens empty) empty)

(define CircleList
  (list 1 2 3 4 5 6 7 8 9))
(define ExCircleList
  (list (circle 1 "solid" "red")
        (circle 2 "solid" "red")
        (circle 3 "solid" "red")
        (circle 4 "solid" "red")
        (circle 5 "solid" "red")
        (circle 6 "solid" "red")
        (circle 7 "solid" "red")
        (circle 8 "solid" "red")
        (circle 9 "solid" "red")))

(define NegCircleList
  (list 3 6 5 3 -1 9 -40 3))

(define ExNegCircleList
  (list (circle 3 "solid" "red")
        (circle 6 "solid" "red")
        (circle 5 "solid" "red")
        (circle 3 "solid" "red")
        empty-image
        (circle 9 "solid" "red")
        empty-image
        (circle 3 "solid" "red")))
(check-expect (red-circles '()) '())
(check-expect (red-circles CircleList) ExCircleList)
(check-expect (red-circles NegCircleList) ExNegCircleList)

(check-expect (explode-digits 0) (list 0))
(check-expect (explode-digits 10) (list 0 1))
(check-expect (explode-digits 123456) (list 6 5 4 3 2 1))
(check-expect (explode-digits 282351) (list 1 5 3 2 8 2))
(check-error (explode-digits -2) "cannot explode negative value")

(check-expect (implode-digits (list 0) 1) 0)
(check-expect (implode-digits (list 0 1) 1) 10)
(check-expect (implode-digits (list 1 5 3 2 8 2)1) 282351)
(check-expect (implode-digits (list 6 5 4 3 2 1)1) 123456)
(check-error (implode-digits (list 6 5 4 3 2 -1)1) "negative digit")
(check-error (implode-digits (list 6 5 4 -3 2 1)1) "negative digit")
(check-error (implode-digits (list 23 1 0)1) "non-decimal digit")
(check-error (implode-digits (list 4 2766 5)1) "non-decimal digit")


;; Problem 2
(check-expect (num-quarters empty) 0)
(check-expect (num-quarters (list 'Dollar 'Quarter 'Nickel 'Dime 'Penny)) 1)
(check-expect (num-quarters (list 'Dollar 'Quarter 'Nickel 'Dime 'Quarter)) 2)
(check-expect (num-quarters (list 'Quarter 'Quarter 'Nickel 'Quarter 'Penny)) 3)
(check-expect (num-quarters (list 'Dollar 'Penny 'Nickel 'Dime 'Penny)) 0)

(check-expect (pct-dimes (list 'Dollar 'Penny 'Nickel 'Dime 'Penny)) 20)
(check-expect (pct-dimes (list 'Dollar 'Penny 'Nickel 'Dollar 'Penny)) 0)
(check-error (pct-dimes empty) "no coins, would divide by zero")
(check-expect (pct-dimes (list 'Dime 'Penny 'Nickel 'Dime 'Penny)) 40)
(check-expect (pct-dimes (list 'Dime 'Penny 'Dime)) 200/3)
(check-expect (pct-dimes (list 'Dime 'Dime 'Dime)) 100)

(check-expect (contains-dollar? (list 'Dime 'Penny 'Nickel 'Dime 'Penny)) #f)
(check-expect (contains-dollar? (list 'Dollar)) #t)
(check-expect (contains-dollar? empty) #f)
(check-expect (contains-dollar? (list 'Penny 'Quarter 'Dollar)) #t)
(check-expect (contains-dollar? (list 'Dollar 'Quarter 'Dime)) #t)
(check-expect (contains-dollar? (list 'Dollar 'Dollar)) #t)

(check-expect (coin-total (list 'Dollar 'Penny 'Penny 'Penny)) 103)
(check-expect (coin-total (list 'Dollar 'Dime 'Nickel 'Quarter 'Penny)) 141)
(check-expect (coin-total (list 'Dime 'Quarter 'Dime 'Nickel 'Quarter 'Penny)) 76)
(check-expect (coin-total empty) 0)

(check-expect (change 0) empty)
(check-expect (change 4) (list 'Penny 'Penny 'Penny 'Penny))
(check-expect (change 5) (list 'Nickel))
(check-expect (change 9) (list 'Nickel 'Penny 'Penny 'Penny 'Penny))
(check-expect (change 10) (list 'Dime))
(check-expect (change 24) (list 'Dime 'Dime 'Penny 'Penny 'Penny 'Penny))
(check-expect (change 25) (list 'Quarter))
(check-expect (change 76) (list 'Quarter 'Quarter 'Quarter 'Penny))
(check-expect (change 74) (list 'Quarter 'Quarter 'Dime 'Dime 'Penny 'Penny 'Penny 'Penny))
(check-expect (change 99) (list 'Quarter 'Quarter 'Quarter 'Dime 'Dime 'Penny 'Penny 'Penny 'Penny))
(check-expect (change 100)(list 'Dollar))
(check-expect (change 104) (list 'Dollar 'Penny 'Penny 'Penny 'Penny))
(check-expect (change 105) (list 'Dollar 'Nickel))
;;== Check this by looking that sizes are decreasing
(show-coins (list 'Dollar 'Quarter 'Nickel 'Dime 'Penny))

;; Problem 3

(define problem3T1 (list (Term -1 5) (Term 1.4 2)))
(define problem3T2 (list (Term -1 5) (Term 1.4 2) (Term 1 1)))
(define problem3T3 (list (Term 0.1 23) (Term 5 5)))
(define problem3T4 (list (Term 0.1 7) (Term 5 5)))

(check-expect (term->string (Term 0 0)) "0")
(check-expect (term->string (Term 5 2)) "5x^2")
(check-expect (term->string (Term 3 4)) "3x^4")
(check-expect (term->string (Term -1 500)) "-1x^500")
(check-expect (term->string (Term 1.5 0)) "1.5")

(check-expect (polynomial->string (list (Term 4 3) (Term 5 2))) "4x^3+5x^2")
(check-expect (polynomial->string problem3T1) "-1x^5+1.4x^2")
(check-expect (polynomial->string problem3T2) "-1x^5+1.4x^2+1x")
(check-expect (polynomial->string problem3T3) "0.1x^23+5x^5")
(check-expect (value-at (list (Term 1 2) (Term 1 1)) 2) 6)
(check-expect (value-at (list (Term 2 2) (Term 1 1)) 2) 10)
(check-within (value-at problem3T1 2) -26.4 .0001)
(check-within (value-at problem3T2 1) 1.4 .0001)
(check-within (value-at problem3T3 -1) -5.1 .001)
(check-expect (derivative (list (Term 4 3) (Term 5 2))) (list (Term 12 2) (Term 10 1)))
(check-within (derivative problem3T1) (list (Term -5 4) (Term 2.8 1)) .00001)
(check-within (derivative problem3T2) (list (Term -5 4) (Term 2.8 1) (Term 1 0)) .0001)
(check-within (derivative problem3T3) (list (Term 2.3 22) (Term 25 4)) .00001)
(check-expect (derivative (list (Term 3 1)(Term 4 0))) (list (Term 3 0) ))

(check-expect (antiderivative (list (Term 5 7)(Term 3 5)(Term 2 1)))
              (list (Term 5/8 8)(Term 1/2 6)(Term 1 2)))
(check-expect (antiderivative (list (Term 2 3)(Term 1 2)(Term 4 1)))
              (list (Term 1/2 4)(Term 1/3 3)(Term 2 2)))
(check-expect (antiderivative (list (Term 1 3)(Term 1 2)))
              (list (Term 1/4 4)(Term 1/3 3)))
(check-expect (antiderivative (list (Term 1 2))) (list(Term 1/3 3)))
(check-expect (antiderivative (list (Term 1 0))) (list(Term 1 1)))
(check-expect (antiderivative empty) empty)


(check-expect (sorted-desc? (list (Term 4 3) (Term 5 2))) true)
(check-expect (sorted-desc? problem3T1) #t)
(check-expect (sorted-desc? problem3T2) #t)
(check-expect (sorted-desc? problem3T3) #t)
(check-expect (sorted-desc? (list (Term 1 -20) (Term 5 -19))) #f)
(check-expect (sorted-desc? (list (Term 1 -19) (Term 5 -20) (Term 1 1))) #f)
(check-expect (sorted-desc? (list (Term 3 9) (Term 1 3) (Term 1 1)(Term 1 8))) #f)

(check-expect (polynomial-add (list (Term 4 3) (Term 5 2)) (list (Term 4 3) (Term 5 2))) (list (Term 8 3 ) (Term 10 2)))
(check-expect (polynomial-add (list (Term 4 3) (Term 5 2)) (list (Term 4 6) (Term 4 3) (Term 5 2))) (list (Term 4 6) (Term 8 3) (Term 10 2)))
(check-within (polynomial-add problem3T1 problem3T2) (list (Term -2 5) (Term 2.8 2) (Term 1 1)) .00001)
(check-within (polynomial-add problem3T1 problem3T3) (list (Term 0.1 23) (Term 4 5) (Term 1.4 2)) .00001)
(check-within (polynomial-add problem3T3 problem3T4) (list (Term 0.1 23) (Term 0.1 7) (Term 10 5)) .00001)

;; problem 1
;; - int-list-sum                     2/ 2
;; - int-list-product                 3/ 3
;; - evens                            2/ 2
;; - red-circles                      2/ 2
;; - explode-digits                   2/ 4
;; - implode-digits                   3/ 4

;; problem 2
;; - num-quarters                     2/ 2
;; - pct-dimes                        2/ 3
;; - contains-dollar?                 2/ 2
;; - coin-total                       2/ 2
;; - change                           4/ 4
;; - show-coins                       3/ 3

;; problem 3
;; - term->string                     2/ 2
;; - polynomial->string               2/ 2
;; - value-at                         2/ 2
;; - derivative                       1/ 2
;; - antiderivative                   2/ 2
;; - sorted-desc?                     4/ 4
;; - polynomial-add                   3/ 5

;; === style ===

;; code layout                        8/ 8
;; identifiers are well named         6/ 6
;; program decomposition (helpers)    4/ 4

;; contracts (type ascriptions)       8/ 8
;; well-written purposes              6/ 6
;; adequate tests                     6/ 6

;; clarity (clear logic)              6/ 6

;; svn used correctly                 4/ 4

;; _total-score_                    93/ 100

;; graded by Sayri Suarez

(test)