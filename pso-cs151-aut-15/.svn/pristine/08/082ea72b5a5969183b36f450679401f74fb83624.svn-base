#lang typed/racket
(require "../include/uchicago151.rkt")
(require typed/test-engine/racket-tests)
(require typed/2htdp/image)
(require typed/2htdp/batch-io)

(define-type (Optional a)
  (U 'none (Some a)))

(define-struct (Some a)
  ([value : a]))

(define-struct Point
  ([x : Real]
   [y : Real]))

(: read-signal : String -> (Listof Number))
;; read in a signal datafile, one number at a time
(define (read-signal filename)
  (local
    {(: word-to-number : String -> Number)
     (define (word-to-number w)
       (local
         {(define n (string->number w))}
         (if (number? n) n (error "not a number"))))}
    (map word-to-number (read-words filename))))
(check-expect (read-signal "test.csv") '(10 20 30 40))

;; plots a positive point
(: plot-point-positive : Number -> Image)
(define (plot-point-positive h)
  (cond
    [(> (real-part h) 0) (rectangle 1 (abs (real-part h)) "solid" "blue")]
    [else (square 1 "solid" (color 128 128 128 0))]))

;;plots all the positive points
(: plot-signal-positive : (Listof Number) -> Image)
(define (plot-signal-positive list)
  (cond
    [(cons? list) (beside/align "bottom" (plot-point-positive (real-part (first list)))
                          (plot-signal-positive (rest list)))]
    [else empty-image]))

;;plots a negative point
(: plot-point-negative : Number -> Image)
(define (plot-point-negative h)
  (cond
    [(< (real-part h) 0) (rectangle 1 (abs (real-part h)) "solid" "blue")]
    [else (square 1 "solid" (color 128 128 128 0))]))

;;plots all the negative points
(: plot-signal-negative : (Listof Number) -> Image)
(define (plot-signal-negative list)
  (cond
    [(cons? list) (beside/align "top" (plot-point-negative (first list))
                          (plot-signal-negative (rest list)))]
    [else empty-image]))

;;changes integer to optional integer
(: optional-integer? : Integer -> (Optional Integer))
(define (optional-integer? x)
  (cond
    [(> x 0) (Some x)]
    [else 'none]))
(check-expect (optional-integer? 10) (Some 10))
                                         
;;plots signal from a list of numbers
(: plot-signal : (Listof Number) (Optional Integer) (Optional Integer) -> Image)
(define (plot-signal list x y)
  (match* (x y)
    [('none 'none) (scale/xy 1 1 (above (plot-signal-positive list)
                                           (plot-signal-negative list)))]
    [('none (Some a)) (if (> (Some-value y) 0)
                          (scale/xy 1 (/ (Some-value y) 1566/100) (above (plot-signal-positive list)
                                            (plot-signal-negative list)))
                          empty-image)]
    [((Some a) 'none) (if (> (Some-value x) 0)
                          (scale/xy (/ 1 (/ 2560 (Some-value x))) 1 (above (plot-signal-positive list)
                                              (plot-signal-negative list)))
                          empty-image)]
    [((Some a) (Some b)) (if (and (> (Some-value x) 0) (> (Some-value y) 0))
                             (scale/xy (/ 1 (/ 2560 (Some-value x))) (/ (Some-value y) 1566/100) (above (plot-signal-positive list)
                                               (plot-signal-negative list)))
                             empty-image)]))

;;finds the nth term fourier transform
(: dft-term : (Listof Number) Integer -> Number)
(define (dft-term list x)
  (* (list-ref list x) (* (exp (/ (- (* (sqrt -1) 2 pi x (- x 1)) (length list)))))))
(check-within (dft-term (list 1 2) 1) 1.21306 0.0001)
(check-within (dft-term (list 1 2) 0) 0.60653 0.0001)

;;finds the list of terms after fourier transformed
(define x -1)
(: dft : (Listof Number) -> (Listof Number))
(define (dft list)
  (match list
    ['() '()]
    [(cons hd '()) (cons (dft-term list (+ x 1)) '())]
    [(cons hd tl) (cons (dft-term list (+ x 1)) (dft tl))]))
(check-within (dft (list 1 2)) (list 0.60653 0.73575) 0.0001)

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

;;creates point from two numbers
(: map-to-point : Integer Real -> Point)
(define (map-to-point x y)
  (Point (/ x 256) y))
(check-expect (map-to-point 1 2) (Point 1/256 2))

;;creates spectrum of points
(: spectrum : (Listof Number) Integer -> (Listof Point))
(define (spectrum list x)
  (index-map  map-to-point (take (map real-part (dft list)) 128)))

;;plots point
(: plot-point-spectrum : Real -> Image)
(define (plot-point-spectrum h)
  (cond
    [(> h 0) (rectangle 1 h "solid" "blue")]
    [else (square 1 "solid" (color 128 128 128 0))]))

;;plots spectrum of points
(: plot-spectrum-positive : (Listof Point) -> Image)
(define (plot-spectrum-positive list)
  (cond
    [(cons? list) (beside/align "bottom" (plot-point-spectrum (Point-y (first list)))
                          (plot-spectrum-positive (rest list)))]
    [else empty-image]))

;;scales and reverses spectrum
(: plot-spectrum : (Listof Point) (Optional Integer) (Optional Integer) -> Image)
(define (plot-spectrum list x y)
  (match* (x y)
    [('none 'none) (beside/align "bottom" (plot-point-spectrum (Point-y (first list)))
                                (plot-spectrum (rest list) x y))]
    [('none (Some a)) (if (> (Some-value y) 0)
                          (scale/xy 1 (/ 8 (Some-value y))
                                    (beside/align "bottom" (plot-point-spectrum (Point-y (first list)))
                                                  (plot-spectrum (rest list) x y)))
                          empty-image)]
    [((Some a) 'none) (if (> (Some-value x) 0)
                          (scale/xy (/ 128 (Some-value x)) 1
                                    (beside/align "bottom" (plot-point-spectrum (Point-y (first list)))
                                                  (plot-spectrum (rest list) x y)))
                          empty-image)]
    [((Some a) (Some b)) (if (and (> (Some-value x) 0) (> (Some-value y) 0))
                              (scale/xy (/ (Some-value x) 128) (/ (Some-value y) 8)
                                       (flip-horizontal (plot-spectrum-positive list)))
                             empty-image)]))

(test)