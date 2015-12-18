#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/uchicago151.rkt")
(require typed/2htdp/image)
(require typed/2htdp/universe)

;;Defines structure ball that has:
;;y position
;;v velocity
;;e elasticity
;;r radius
;;c color of ball
(define-struct Ball
  ([y : Real]
   [v : Real]
   [e : Real]
   [r : Real]
   [c : Image-Color]))

;;Defines structure world that contains a list of balls and a counter
(define-struct World
  ([balls : (Listof Ball)]
   [counter : Integer]))

;;Takes in object ball and draws an image that corresponds to its properties
(: draw-ball : Ball -> Image)
(define (draw-ball b)
  (cond
    [(and (> (Ball-r b) 0) (Ball? b) (>= (Ball-y b) 0))
     (above (circle (Ball-r b) "solid" (Ball-c b))
                    (rectangle (* 2 (Ball-r b)) (Ball-y b) "solid" "ivory"))]
    [else empty-image]))

;;Takes in a list of balls and draws an image that corresponds to each ball
;;with a separator between each ball
(: draw-balls : (Listof Ball) -> Image)
(define (draw-balls list)
  (cond
    [(cons? list) (beside/align "bottom" (draw-ball (first list))
                                (rectangle 50 600 "solid" "ivory")
                  (draw-balls (rest list)))]
    ['() empty-image]))

;;Takes in a world and draws an image
(: draw : World -> Image)
(define (draw w)
  (overlay/align "middle" "bottom" (draw-balls (World-balls w))
           (overlay/align "right" "bottom"
                          (text (string-append "Time: " (number->string (World-counter w))) 16 "black")
                          (square 600 "solid" "ivory"))))
;;Gravitational constant
(define g -9.8)

;;Changes the position y and velocity v of a ball
(: change-yv : Ball -> Ball)
(define (change-yv b)
  (match b
    [(Ball y v e r c) (cond
                        [(and (> y 0) (> (+ (Ball-y b) (Ball-v b)) 0))
                         (Ball (+ (Ball-y b) (Ball-v b))
                               (+ (Ball-v b) g)
                               e
                               r
                               c)]
                        [(and (> y 0) (<= (+ (Ball-y b) (Ball-v b)) 0))
                         (Ball 0 (- (Ball-v b) g) e r c)]
                        [(= y 0) (Ball
                                  (max 0.0 (- (- (* e v)) 0.5))
                                  (max 0.0 (- (- (* e v)) 0.5))
                                  e
                                  r
                                  c)]
                        [else b])]
    [else b]))
(check-within (change-yv (Ball 500 0 0.8 50 "slategray")) (Ball 500 -9.8 0.8 50 "slategray") 0.01)
(check-within (change-yv (Ball 0 -20 0.8 50 "slategray")) (Ball 15.5 15.5 0.8 50 "slategray") 0.01)
(check-within (change-yv (Ball 0 0 0.8 50 "slategray")) (Ball 0 0 0.8 50 "slategray") 0.01)

;;Changes the list of balls with a new list of balls with new positions
;;and velocities
(: change-list : (Listof Ball) -> (Listof Ball))
(define (change-list list)
  (cond
    [(cons? list) (cons (change-yv (first list))
                        (change-list (rest list)))]
    [else list]))
(check-within (change-list (list
                            (Ball 500 0 0.8 50  "slategray")
                            (Ball 400 0 0.7 25  "hotpink")))
                           (list
                            (Ball 500 -9.8 0.8 50  "slategray")
                            (Ball 400 -9.8 0.7 25  "hotpink")) 0.01)

;;Changes the counter of the world at every 1/28th of a second
(: tick : World -> World)
(define (tick w)
  (match w
    [(World b c) (World (change-list b) (add1 c))]
    [else w]))
(check-within (tick (World (list
                            (Ball 500 0 0.8 50  "slategray")
                            (Ball 400 0 0.7 25  "hotpink"))
                           12))
              (World (list
                            (Ball 500 -9.8 0.8 50  "slategray")
                            (Ball 400 -9.8 0.7 25  "hotpink"))
                           13) 0.01)

;;Gets all the velocities from a list of balls and returns the absolute value
;;of the velocities in a list of reals
(: get-all-v : (Listof Ball) -> (Listof Real))
(define (get-all-v list)
  (cond
    [(cons? list) (cons (abs (Ball-v (first list))) (get-all-v (rest list)))]
    ['()'()]))
(check-within (get-all-v (list
                          (Ball 500 -9.8 0.8 50  "slategray")
                          (Ball 400 -9.8 0.7 25  "hotpink")))
              (list 9.8 9.8) 0.01)
(check-expect (get-all-v '()) '())

;;Gets all the positions from a list of balls and returns the positions in a list
;;of reals
(: get-all-y : (Listof Ball) -> (Listof Real))
(define (get-all-y list)
  (cond
    [(cons? list) (cons (Ball-y (first list)) (get-all-y (rest list)))]
    ['()'()]))
(check-within (get-all-y (list
                          (Ball 500 -9.8 0.8 50  "slategray")
                          (Ball 400 -9.8 0.7 25  "hotpink")))
              (list 500 400) 0.01)
(check-expect (get-all-y '()) '())

;;Checks whether the balls are on the floor and have zero velocity
;;Stops world if so
(: finished? : World -> Boolean)
(define (finished? w)
  (if (and (= (apply max (get-all-v (World-balls w))) 0)
           (= (apply max (get-all-y (World-balls w))) 0))
      #t
      #f))
(check-expect (finished? (World (list
                                 (Ball 500 -9.8 0.8 50  "slategray")
                                 (Ball 400 -9.8 0.7 25  "hotpink"))
                                1))
              #f)
(check-expect (finished? (World (list
                                 (Ball 0 0 0.8 50  "slategray")
                                 (Ball 0 0 0.7 25  "hotpink"))
                                1))
              #t)

(test)

(big-bang (World (list (Ball 500 0 0.8 50  "slategray")
                       (Ball 400 0 0.7 25  "hotpink")
                       (Ball 300 10 0.1 15 "aqua")
                       (Ball 200 25 0.4 20 "wheat")) 0) : World
          [to-draw draw]
          [on-tick tick]
          [stop-when finished?])
          