#lang typed/racket
(require typed/test-engine/racket-tests)

;;Checks if input year is a leap year
(: leap-year? (-> Integer Boolean))
(define (leap-year? year)
  (if (or (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))
   (= (modulo year 400) 0)) #t #f))
(check-expect (leap-year? 2016) #t)
  
;;Checks if the input m,d,y is a valid date

;;grader checks year everytime
(: valid-date? (-> Integer Integer Integer Boolean))
(define (valid-date? m d y)
  (cond
    ;;grader; add year range here so it does not need to be done everytime
    ;;could move like months together
    [(and (= m 1) (<= d 31) (>= d 1) (<= y 2099) (>= y 1900)) #t]
    [(and (= m 2) (leap-year? y) (<= d 29) (>= d 1) (<= y 2099) (>= y 1900)) #t]
    [(and (= m 2) (not (leap-year? y)) (<= d 28) (>= d 1) (<= y 2099) (>= y 1900)) #t]
    [(and (= m 3) (<= d 31) (>= d 1) (<= y 2099) (>= y 1900)) #t]
    [(and (= m 4) (<= d 30) (>= d 1) (<= y 2099) (>= y 1900)) #t]
    [(and (= m 5) (<= d 31) (>= d 1) (<= y 2099) (>= y 1900)) #t]
    [(and (= m 6) (<= d 30) (>= d 1) (<= y 2099) (>= y 1900)) #t]
    [(and (= m 7) (<= d 31) (>= d 1) (<= y 2099) (>= y 1900)) #t]
    [(and (= m 8) (<= d 31) (>= d 1) (<= y 2099) (>= y 1900)) #t]
    [(and (= m 9) (<= d 30) (>= d 1) (<= y 2099) (>= y 1900)) #t]
    [(and (= m 10) (<= d 31) (>= d 1) (<= y 2099) (>= y 1900)) #t]
    [(and (= m 11) (<= d 30) (>= d 1) (<= y 2099) (>= y 1900)) #t]
    [(and (= m 12) (<= d 31) (>= d 1) (<= y 2099) (>= y 1900)) #t]
    [else #f]))
(check-expect (valid-date? 3 31 2016) #t)
;;grader: create more tests

;;returns month adjustment value, j, if date is a leap year
(: is-leap-year (-> Integer Integer))
(define (is-leap-year j)
  (cond
    [(= j 1) 0]
    [(= j 2) 3]
    [(= j 3) 4]
    [(= j 4) 0]
    [(= j 5) 2]
    [(= j 6) 5]
    [(= j 7) 0]
    [(= j 8) 3]
    [(= j 9) 6]
    [(= j 10)1]
    [(= j 11) 4]
    [(= j 12) 6]
    [else 0]))
(check-expect (is-leap-year 3) 4)

;;grader: could combine both functions into one

;;returns month adjustment value, j, if date is not a leap year
(: is-not-leap-year (-> Integer Integer))
(define (is-not-leap-year j)
  (cond
    [(= j 1) 1]
    [(= j 2) 4]
    [(= j 3) 4]
    [(= j 4) 0]
    [(= j 5) 2]
    [(= j 6) 5]
    [(= j 7) 0]
    [(= j 8) 3]
    [(= j 9) 6]
    [(= j 10)1]
    [(= j 11) 4]
    [(= j 12) 6]
    [else 0]))
(check-expect (is-not-leap-year 1) 1)

;;converts value to string for day of week
(: convert-to-string (-> Integer String))
(define (convert-to-string d)
  (cond
    [(= d 0) "Sunday"]
    [(= d 1) "Monday"]
    [(= d 2) "Tuesday"]
    [(= d 3) "Wednesday"]
    [(= d 4) "Thursday"]
    [(= d 5) "Friday"]
    [(= d 6) "Saturday"]
    [else "Error"]))
(check-expect (convert-to-string 1) "Monday")

;;Divides input by 7
(: month-adjustment (-> Integer Integer))
(define (month-adjustment m)
 ; (modulo m 7));;original code
   (remainder m 7));;grader added 
(check-expect (month-adjustment 8) 1)

;;Requests input m d y and returns day of week of the date
(: day-of-week (-> Integer Integer Integer String))
;;grader: does not work. check inline notes
;;
(define (day-of-week m d y)
  (if; (leap-year? m);;original code
   (leap-year? y);;grader: corrected here
      (convert-to-string (month-adjustment (+ (- y 1900) (is-leap-year m) d (exact-floor(/ y 4)))))
      (convert-to-string (month-adjustment (+ (- y 1900) (is-not-leap-year m) d (exact-floor(/ y 4)))))))
(check-expect (day-of-week 10 6 2015) "Tuesday")

;;converts input m and returns string of month
(: convert-to-month (-> Integer String))
(define (convert-to-month m)
  (cond
    [(= m 1) "January"]
    [(= m 2) "February"]
    [(= m 3) "March"]
    [(= m 4) "April"]
    [(= m 5) "May"]
    [(= m 6) "June"]
    [(= m 7) "July"]
    [(= m 8) "August"]
    [(= m 9) "September"]
    [(= m 10) "October"]
    [(= m 11) "November"]
    [(= m 12) "December"]
    [else "Error"]))
(check-expect (convert-to-month 3) "March")

;;Requests input m d y and returns string value of date & day of week
;;grader: does not check if date is invalid
(: date->string (-> Integer Integer Integer String))
(define (date->string m d y)
  (string-append (convert-to-month m) " " (number->string d) ", " (number->string y) " (" (day-of-week m d y) ")"))
(check-expect (date->string 10 6 2015) "October 6, 2015 (Tuesday)")

(test)

;;graded added tests that fail

(check-expect(day-of-week 2 19 1940) "Monday")
(check-expect (day-of-week 1 12 2000) "Wednesday")

;(check-expect (date->string 01 32 2042) "[invalid date]")
;(check-expect (date->string 2 29 2015) "[invalid date]")
;(check-expect (date->string 2 29 2012) "February 29, 2012 (Wednesday)")
;(check-expect (date->string 6 32 1980) "[invalid date]")
(test)


;;Grader: See in-line comments
;; ====== correctness

;; === correctness ===

;; leap-year?   - /  6
;; valid-date?  -/ 10
;; day-of-week  5/ 10
;; date-string  6/  8

;; === style ===

;; code layout                       -/ 10
;; identifiers are well named        -/ 10
;; program decomposition (helpers)   8/ 10
;; contracts (type ascriptions)      -/  8
;; well-written purposes             -/  8
;; adequate tests                    2/  8 
;; clarity (clear logic)             -/  8

;; svn used correctly                 -/  4

;; _total-score_                     85/ 100

;; graded by Charisee Chiw
