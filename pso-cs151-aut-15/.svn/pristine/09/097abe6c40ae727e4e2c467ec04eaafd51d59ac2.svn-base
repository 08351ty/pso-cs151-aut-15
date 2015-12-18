#lang typed/racket
(require typed/test-engine/racket-tests)
(require typed/2htdp/image)
(require "../include/uchicago151.rkt")

(define-struct (Some A)
  ([value : A]))

(define-type (Optional A) 
  (U 'none (Some A)))

(define-struct Point
  ([x : Real] 
   [y : Real]))

(define-struct Line
  ([m : Real]
   [b : Real]))

(define-struct (Node A)
  ([root : A]
   [lsub : (Tree A)]
   [rsub : (Tree A)]))

(define-type (Tree A) 
  (U 'empty (Node A)))

(define-struct Interval
  ([min : Integer]
   [max : Integer]))

;;Sums the magnitude of each index in list
(: sum-magnitude : (Listof Real) -> Real)
(define (sum-magnitude list)
  (foldr + 0 (map abs list)))
(check-expect (sum-magnitude (list 1 4 6 7 -1)) 19)

;;Creates a point from two reals
(: create-point : Real Real -> Point)
(define (create-point x y)
  (Point x y))
(check-expect (create-point 2 3) (Point 2 3))

;;evaluates function at specified x value and returns a list of points with x and y value
(: eval-f-at : (Real -> Real) (Listof Real) -> (Listof Point))
(define (eval-f-at f list)
  (map (λ ([n : Real]) (create-point n (f n))) list))
(check-expect (eval-f-at add1 (list 2 3 4)) (list (Point 2 3) (Point 3 4) (Point 4 5)))

;;determines whether a real is within the interval. Returns boolean value
(: within-bounds? : Interval Real -> Boolean)
(define (within-bounds? i x)
  (if (<= (Interval-min i) x (Interval-max i))
      #t
      #f))
(check-expect (within-bounds? (Interval -10 10) 3) #t)
(check-expect (within-bounds? (Interval -10 10) -11) #f)
(check-expect (within-bounds? (Interval -1000 10) -999) #t)

;;outputs a list of reals that are within the interval specified
(: in-range : Interval (Listof Real) -> (Listof Real))
(define (in-range i list)
  (filter (λ ([n : Real]) (within-bounds? i n)) list))
(check-expect (in-range (Interval -10 10) (list 1 2 3 4)) (list 1 2 3 4))
(check-expect (in-range (Interval -10 10) (list 10 20 3 40)) (list 10 3))

;;helper function for implode digits
(: proper-size : (Listof Integer) Integer -> (Listof Integer))
(define (proper-size list x)
  (cond
    [(cons? list) (cons (* (first list) x) (proper-size (rest list) (* x 10)))]
    [else '()]))
(check-expect (proper-size (list 1 2 3) 1) (list 1 20 300))

;;adds a list of integers where first element is unit, second is tens, third is hundreds etc.
(: implode-digits : (Listof Integer) -> Integer)
(define (implode-digits list)
  (foldr + 0 (proper-size list 1)))
(check-expect (implode-digits (list 3 2 1 4)) 4123)
(check-expect (implode-digits (list 9 8 7 6)) 6789)

;; helper function for on-line
(: on-line? : Line Point -> Boolean)
(define (on-line? line p)
  ;; This should be +, not -
  (if (= (Point-y p) (- (* (Line-m line) (Point-x p)) (Line-b line)))
      #t
      #f))
(check-expect (on-line? (Line 3 0) (Point 10 10)) #f)
(check-expect (on-line? (Line 3 0) (Point 10 30)) #t)

;;returns the list of points that are on the line given
(: on-line : Line (Listof Point) -> (Listof Point))
(define (on-line line list)
  (filter (λ ([x : Point]) (on-line? line x)) list))
(check-expect (on-line (Line 3 0) (list (Point 10 30) (Point 10 10))) (list (Point 10 30)))

;;finds the first in list that passes test. If none in list, return 'none
(: find : All (A) (A -> Boolean) (Listof A) -> (Optional A))
(define (find t list)
  (match (filter t list)
    ['() 'none]
    [(cons hd '()) (Some hd)]
    [(cons hd tl) (Some hd)]))
(check-expect (find positive? (list 2 5 7 -1 -5 -6 -7 -10)) (Some 2))
(check-expect (find positive? (list -2 -5 -7 -1 -5 -6 -7 -10)) 'none)
(check-expect (find positive? (list -2 -5 -7 -1 -5 -6 -7 10)) (Some 10))

;;maps over a list, if test passes, adds new value to list, else add original value
(: condmap : All (A) (A -> Boolean) (A -> A) (Listof A) -> (Listof A))
(define (condmap t f list)
  (cond
    [(cons? list) (cons (if (t (first list))
                            (f (first list))
                            (first list))
                        (condmap t f (rest list)))]
    [else '()]))
(check-expect (condmap positive? sqr (list 1 -5 6 -8 10)) (list 1 -5 36 -8 100))

;;given equality function, find and replace value, replaces find value in list with replace value
(: find-replace : All (A) (A A -> Boolean) A A (Listof A) -> (Listof A))
(define (find-replace t find replace list)
  (local
    {(define (pass-test find) replace)
     (define (equality? [x : A]) (if (t x find)
                                 #t
                                 #f))}
  (cond
    [(cons? list) (condmap equality?
                           pass-test
                           list)]
    ['() '()])))
(check-expect (find-replace = 1 -1 (list 3 2 1 0 -1 -2 -3)) (list 3 2 -1 0 -1 -2 -3))
(check-expect (find-replace = 1 -100 (list 3 2 1 0 -1 -2 -3)) (list 3 2 -100 0 -1 -2 -3))
(check-expect (find-replace symbol=? 'hello 'goodbye (list 'hello 'you)) (list 'goodbye 'you))

(define i -1)
;;maps over all elements of list with index
(: index-map : All (A B) (Integer A -> B) (Listof A) -> (Listof B))
(define (index-map f list)
  (cond
    [(cons? list) (set! i (+ i 1)) (cons (f i (first list)) (index-map f (rest list)))]
    [else '()]))
(check-expect (index-map
               (λ ([i : Integer] [s : String])
                 (string-append "At index " (number->string i) " there was " s))
               (list "A" "B" "Z"))
              '("At index 0 there was A" "At index 1 there was B" "At index 2 there was Z"))

;;replaces element with new element at specified index
(: replace-at : All (A) Integer A (Listof A) -> (Listof A))
(define (replace-at x a list)
  (index-map (λ ([n : Integer] [s : A]) (match n
                                          [x a]
                                          [_ s])) list))
(check-expect (replace-at
               1 "C" (list "C" "A" "C"))
              (list "C" "C" "C"))

;;takes in two lists and folding function and returns the total folding result
(: foldrpair : All (A B C) (A B C -> C) C (Listof A) (Listof B) -> C)
(define (foldrpair f res list1 list2)
  (cond
    [(and (cons? list1)
          (cons? list2)
          (= (length list1) (length list2))) (foldrpair f (f (first list1) (first list2) res) (rest list1) (rest list2))]
    [else res]))
(check-expect (foldrpair
  (λ ([x : Real] [y : Real] [acc : Point])
    (match acc
      [(Point xs ys) (Point (+ x xs) (+ y ys))]))
  (Point 0 0)
  (list 1 2 3) (list 100 200 300)) (Point 6 600))

;;uses foldrpair to calculate weighted arithmetic mean
(: weighted-mean : (Listof Real) (Listof Real) -> Real)
(define (weighted-mean list1 list2)
  (foldrpair (λ ([x : Real] [y : Real] [acc : Real])
               (+ (* x y) acc))
             0 list1 list2))
(check-expect (weighted-mean (list 1 6) (list 3 10)) 63)
(check-expect (weighted-mean (list 10 20 30) (list 2 4 8)) 340)

;;helper function for filter-pair
(: optional-A : All (A) (Optional A) -> A)
(define (optional-A a)
  (match a
    ['none (error "invalid")]
    [(Some a) a]))
(check-expect (optional-A (Some 3)) 3)

;;takes in two lists and a filtering function and returns result of function or nothing, creating list
(: filter-pair : All (A) (A A -> (Optional A)) (Listof A) (Listof A) -> (Listof A))
(define (filter-pair f list1 list2)
  (match list1
    [(cons hd1 tl1) (match list2
                      [(cons hd2 tl2) (if (eq? (f hd1 hd2) 'none)
                                          (filter-pair f tl1 tl2)
                                          (cons (optional-A (f hd1 hd2))
                                                (filter-pair f tl1 tl2)))])]
                                              
    [else '()]))
;;Need to acocunt for lists of different lengths
(check-expect (filter-pair winning-score (list 1 2 3) (list 2 3 4))
              (list 2 3 4))

;;helper function for winning-scores
(: winning-score : Integer Integer -> (Optional Integer))
(define (winning-score x y)
  (cond
    [(> x y) (Some x)]
    [(> y x) (Some y)]
    [else 'none]))
(check-expect (winning-score 2 3) (Some 3))
(check-expect (winning-score 3 3) 'none)

;;takes in two lists of scores and returns list of winning side's score
(: winning-scores : (Listof Integer) (Listof Integer) -> (Listof Integer))
(define (winning-scores list1 list2)
  (filter-pair winning-score list1 list2))
(check-expect (winning-scores (list 1 2 3 2 1) (list 5 5 3 1 2)) (list 5 5 2 2))
(check-expect (winning-scores (list 10 2 3 2 1) (list 10 2 3 2 1)) '())

;;traverses tree and evaluates each node through function
(: treemap : All (A B) (A -> B) (Tree A) -> (Tree B))
(define (treemap f t)
  (cond
    [(Node? t) (Node (f (Node-root t))
                     (treemap f (Node-lsub t))
                     (treemap f (Node-rsub t)))]
    [else 'empty]))
(check-expect (treemap (λ [(n : Integer)] (+ n 2)) (Node 5 (Node 3 'empty 'empty)
                                                         (Node 2 'empty 'empty)))
              (Node 7 (Node 5 'empty 'empty) (Node 4 'empty 'empty)))

;;using treemap converts tree of booleans to strings, where s1 is true value and s2 false value
(: bool-tree->string-tree : String String (Tree Boolean) -> (Tree String))
(define (bool-tree->string-tree s1 s2 t)
  (local
    {(: bool->string : Boolean -> String)
     (define (bool->string b)
       (match b
         [#t s1]
         [#f s2]))}
  (treemap bool->string t)))
(check-expect (bool-tree->string-tree "yay" "noooo" (Node #t (Node #t 'empty 'empty)
                                                          (Node #f 'empty 'empty)))
              (Node "yay" (Node "yay" 'empty 'empty)
                    (Node "noooo" 'empty 'empty)))
                              
                                            

(test)

;; ====== correctness

;; === correctness ===

;; problem 1
;; - sum-magnitude                   3 / 3
;; - eval-f-at                       2 / 2
;; - in-range                        2 / 2
;; - implode-digits                  3 / 3
;; - on-line                         2 / 3
;; - find                            3 / 3

;; problem 2
;; - condmap                         4 / 4
;; - find-replace                    3 / 3
;; - index-map                       4 / 4
;; - replace-at                      3 / 3
;; - foldr-pair                      4 / 4
;; - weighted-mean                   3 / 3
;; - filter-pair                     3 / 4
;; - winning-scores                  3 / 3
;; - treemap                         4 / 4
;; - bool-tree->string-tree          3 / 3

;; === style ===

;; code layout                       8 / 8
;; identifiers are well named        6 / 6
;; program decomposition (helpers)   4 / 4

;; contracts (type ascriptions)      8 / 8
;; well-written purposes             6 / 6
;; adequate tests                    3 / 6

;; clarity (clear logic)             5 / 6

;; svn used correctly                4 / 4

;; _total-score_                   94 / 100

;; graded by bobni

  
  