#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/uchicago151.rkt")
;;Problem 1
;;Converts an exact-rational in inches to meters
(: in->m (-> Exact-Rational Exact-Rational))
(define (in->m in)
  (* in 254/10000))
(check-expect (in->m 1)254/10000)

;;Converts an exact-rational in meters to inches
(: m->in (-> Exact-Rational Exact-Rational))
(define (m->in m)
  (* m 10000/254))
(check-expect (m->in 1)10000/254)

;;Converts feet and inches to meters
(: ft-in->m (-> Integer Exact-Rational Exact-Rational))
(define (ft-in->m ft in)
  (* (+ (* ft 12) in) 254/10000))
(check-expect (ft-in->m 6 0) 18288/10000)

;;Calculates the area under the quadratic function from 0 to n
(: area-under-quad (-> Real Real Real Real Real))
(define (area-under-quad a b c n)
  (+ (* (/ a 3) (* n n n))
     (* (/ b 2) (* n n))
     (* c n)))
(check-within (area-under-quad 3 2 1 4) 84 0.00001)

;;Problem 2
;;Determines if input body temperature is consistent with
;;having a fever
(: febrile? (-> Real Boolean))
(define (febrile? a)
  (> a 99.5))
(check-expect (febrile? 99.6) #t)

;;Test whether input would be written as a seven-digit number
;;in standard decimal notation
(: seven-digits? (-> Integer Boolean))
(define (seven-digits? a)
  (and (>= a 1000000) (< a 10000000)))
(check-expect (seven-digits? 9999999) #t)

;;Takes the number of X's and O's on a 3x3 tic-tac-toe board
;;and determines whether the counts are consistent with the
;;size of the board and the rules of the game
(: valid-ttt? (-> Integer Integer Boolean))
(define (valid-ttt? x o)
  ;;there should be three qualifications:
  ;;1) x is less than or equal to 5, o is less than or equal to 4
  ;;2) x is greater than o by 1 or equal to o
  ;;3) both x and o must be greater than or equal to 0.
  (and (and (<= x 5) (<= o 4))
       (or (= (+ o 1) x) (= o x))
       (or (>= x 0) (>= o 0))))
(check-expect (valid-ttt? 4 2) #f)

;;Problem 3
;;Load ventra card with enough money for:
;;x amount of train trips
;;y amount of bus trips
;;z amount of transfers
(: add-to-ventra (-> Integer Integer Integer Integer))
(define (add-to-ventra x y z)
  (* (exact-ceiling (/ (+ (* 2.25 x) (* 2 y) (* 0.25 z)) 5)) 5))
(check-expect (add-to-ventra 2 3 4) 15)

;;Problem 4
;;Calculate line of sight
(: offing-line-of-sight/meters (-> Real Real))
(define (offing-line-of-sight/meters h)
  ;;Denote line of sight as s.
  ;;Using pythagoras' theorem, s^2 + r^2 = (h + r)^2
  (real-sqrt (+ (* 2 6371000 h) (* h h))))
(check-within (offing-line-of-sight/meters 2) 5048 1)

;;Calculate ground distance
(: offing-ground-distance/meters (-> Real Real))
(define (offing-ground-distance/meters h)
  (* 6371000 (acos (/ 6371000 (+ 6371000 h)))))
(check-within (offing-ground-distance/meters 6371000) 6671700 100) 

;;Calculate line of sight in miles
(: offing-line-of-sight/miles (-> Real Real))
(define (offing-line-of-sight/miles h)
  (* (offing-line-of-sight/meters h) 0.000621371))
(check-within (offing-line-of-sight/miles 2) 3.136682 0.1)

;;Calculate ground distance in miles
(: offing-ground-distance/miles (-> Real Real))
(define (offing-ground-distance/miles h)
  (* (offing-ground-distance/meters h) 0.000621371))
(check-within (offing-ground-distance/miles 6371000) 4145.602183 10)


;; === grader's tests ===

;; problem 1

(check-expect (in->m 0) 0)
(check-expect (in->m -5) -127/1000)
(check-expect (in->m 10) 127/500)
(check-expect (m->in 1) 10000/254)
(check-expect (m->in 1/16) 625/254)
(check-expect (m->in 5/2) 12500/127)
(check-expect (ft-in->m 5 10) 889/500)
(check-expect (ft-in->m 15 2) 11557/2500)
(check-expect (ft-in->m 1 0) 381/1250)
(check-expect (area-under-quad 5 7 9 11) 16445/6)
(check-expect (area-under-quad 0 0 0 0) 0)
(check-expect (area-under-quad 1 0 -7 4) -20/3)

;; problem 2

(check-expect (febrile? 100) #t)
(check-expect (febrile? 99.5) #f)
(check-expect (febrile? 97) #f)
(check-expect (febrile? 0) #f)
(check-expect (seven-digits? 999999) #f)
(check-expect (seven-digits? 1000000) #t)
(check-expect (seven-digits? 1234567) #t)
(check-expect (seven-digits? 3000000) #t)
(check-expect (seven-digits? 9999999) #t)
(check-expect (seven-digits? 123) #f)
(check-expect (seven-digits? 0) #f)
(check-expect (valid-ttt? 5 3) #f)
(check-expect (valid-ttt? 4 3) #t)
(check-expect (valid-ttt? 0 0) #t)
(check-expect (valid-ttt? 1 1) #t)
(check-expect (valid-ttt? 2 2) #t)
(check-expect (valid-ttt? 5 5) #f)
(check-expect (valid-ttt? 5 4) #t)
(check-expect (valid-ttt? 2 1) #t)
(check-expect (valid-ttt? 1 2) #f)
(check-expect (valid-ttt? 6 5) #f)
(check-expect (valid-ttt? 12 11) #f)

;; problem 3

(check-expect (add-to-ventra 0 0 0) 0)
(check-expect (add-to-ventra 1 1 1) 5)
(check-expect (add-to-ventra 4 2 4) 15)
(check-expect (add-to-ventra 4 2 8) 15)
(check-expect (add-to-ventra 4 2 9) 20)
(check-expect (add-to-ventra 99 99 99) 450)

;; problem 4

(check-within (offing-line-of-sight/meters 1) 3569.5939 0.0001)
(check-within (offing-line-of-sight/meters 2) 5048.1683 0.0001)
(check-within (offing-line-of-sight/meters 3) 6182.7185 0.0001)
(check-within (offing-line-of-sight/meters 20) 15963.7213 0.0001)
(check-within (offing-line-of-sight/meters 0) 0 0.0001)
(check-within (offing-line-of-sight/meters 135) 41475.1518 .0001)

(check-within (offing-ground-distance/meters 1) 3569.5935 0.0001)
(check-within (offing-ground-distance/meters 2) 5048.1673 0.0001)
(check-within (offing-ground-distance/meters 3) 6182.7166 0.0001)
(check-within (offing-ground-distance/meters 20) 15963.6879 0.0001)
(check-within (offing-ground-distance/meters 0) 0 0.0001)
(check-within (offing-ground-distance/meters 135) 41474.5660 .0001)

(check-within (offing-line-of-sight/miles 1) 2.2180 0.0001)
(check-within (offing-line-of-sight/miles 2) 3.1367 0.0001)
(check-within (offing-line-of-sight/miles 3) 3.8417 0.0001)
(check-within (offing-line-of-sight/miles 20) 9.9193 0.0001)
(check-within (offing-line-of-sight/miles 0) 0 0.0001)
(check-within (offing-line-of-sight/miles 135) 25.7714 .0001)

(check-within (offing-ground-distance/miles 1) 2.2180 0.0001)
(check-within (offing-ground-distance/miles 2) 3.1367 0.0001)
(check-within (offing-ground-distance/miles 3) 3.8417 0.0001)
(check-within (offing-ground-distance/miles 20) 9.9193 0.0001)
(check-within (offing-ground-distance/miles 0) 0 0.0001)
(check-within (offing-ground-distance/miles 135) 25.7710 .0001)

(test)

;; ====== correctness

;; === correctness ===

;; problem 1
;; - in->m                            3 / 3
;; - m->in                            3 / 3
;; - ft-in->m                         4 / 4
;; - area-under-quad                  6 / 6

;; problem 2
;; - febrile?                         4 / 4
;; - seven-digits?                    4 / 4 
;; - valid-ttt?                       6 / 6 

;; problem 3
;; - add-to-ventra                    6 / 6

;; problem 4
;; - offing-line-of-sight/meters      5 / 5
;; - offing-ground-distance/meters    5 / 5
;; - offing-line-of-sight/miles       3 / 3
;; - offing-ground-distance/miles     3 / 3

;; === style ===

;; code layout                        8 / 8
;; identifiers are well named         6 / 6
;; program decomposition (helpers)    4 / 4

;; contracts (type ascriptions)       8 / 8
;; well-written purposes              6 / 6
;; adequate tests                     3 / 6 (please write more tests, to catch all cases)

;; clarity (clear logic)              6 / 6

;; svn used correctly                 4 / 4

;; _total-score_                    97 / 100

;; graded by Kunal Goyal
