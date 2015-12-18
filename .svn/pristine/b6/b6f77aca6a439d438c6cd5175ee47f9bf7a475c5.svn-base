#lang typed/racket
(require "../include/uchicago151.rkt")
(require typed/test-engine/racket-tests)
(require typed/2htdp/image)
(require typed/2htdp/universe)

;;PENTAGO/4
;;One quadrant of pentago. Basically tic tac toe except there's an
;;option of rotating the board and a timer

;;defines structure world that tracks x and y co-ordinates of mouse
;;nine circle states, whose turn it is, and counter for the timer
(define-struct World
  ([x : Integer]
   [y : Integer]
   [r1c1 : Integer]
   [r2c1 : Integer]
   [r3c1 : Integer]
   [r1c2 : Integer]
   [r2c2 : Integer]
   [r3c2 : Integer]
   [r1c3 : Integer]
   [r2c3 : Integer]
   [r3c3 : Integer]
   [player-one : Boolean]
   [counter : Integer]))

;;Draws the board, circles and timer
(: draw : World -> Image)
(define (draw w)
  (overlay (above (beside
                   (above
                    (rectangle 80 15 "solid" "ivory")
                    (cond
                      [(= (World-r1c1 w) 0) (circle 40 "outline" "black")]
                      [(= (World-r1c1 w) 1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "salmon"))]
                      [(= (World-r1c1 w) -1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "cyan"))]
                      [else empty-image])
                    (rectangle 80 15 "solid" "ivory")
                    (cond
                      [(= (World-r2c1 w) 0) (circle 40 "outline" "black")]
                      [(= (World-r2c1 w) 1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "salmon"))]
                      [(= (World-r2c1 w) -1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "cyan"))]
                      [else empty-image])
                    (rectangle 80 15 "solid" "ivory")
                    (cond
                      [(= (World-r3c1 w) 0) (circle 40 "outline" "black")]
                      [(= (World-r3c1 w) 1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "salmon"))]
                      [(= (World-r3c1 w) -1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "cyan"))]
                      [else empty-image])
                    (rectangle 80 15 "solid" "ivory"))
                   (rectangle 15 80 "solid" "ivory")
                   (above
                    (rectangle 80 15 "solid" "ivory")
                    (cond
                      [(= (World-r1c2 w) 0) (circle 40 "outline" "black")]
                      [(= (World-r1c2 w) 1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "salmon"))]
                      [(= (World-r1c2 w) -1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "cyan"))]
                      [else empty-image])
                    (rectangle 80 15 "solid" "ivory")
                    (cond
                      [(= (World-r2c2 w) 0) (circle 40 "outline" "black")]
                      [(= (World-r2c2 w) 1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "salmon"))]
                      [(= (World-r2c2 w) -1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "cyan"))]
                      [else empty-image])
                    (rectangle 80 15 "solid" "ivory")
                    (cond
                      [(= (World-r3c2 w) 0) (circle 40 "outline" "black")]
                      [(= (World-r3c2 w) 1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "salmon"))]
                      [(= (World-r3c2 w) -1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "cyan"))]
                      [else empty-image])
                    (rectangle 80 15 "solid" "ivory"))
                   (rectangle 15 80 "solid" "ivory")
                   (above
                    (rectangle 80 15 "solid" "ivory")
                    (cond
                      [(= (World-r1c3 w) 0) (circle 40 "outline" "black")]
                      [(= (World-r1c3 w) 1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "salmon"))]
                      [(= (World-r1c3 w) -1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "cyan"))]
                      [else empty-image])
                    (rectangle 80 15 "solid" "ivory")
                    (cond
                      [(= (World-r2c3 w) 0) (circle 40 "outline" "black")]
                      [(= (World-r2c3 w) 1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "salmon"))]
                      [(= (World-r2c3 w) -1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "cyan"))]
                      [else empty-image])
                    (rectangle 80 15 "solid" "ivory")
                    (cond
                      [(= (World-r3c3 w) 0) (circle 40 "outline" "black")]
                      [(= (World-r3c3 w) 1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "salmon"))]
                      [(= (World-r3c3 w) -1) (overlay (circle 40 "outline" "black") (circle 40 "solid" "cyan"))]
                      [else empty-image])
                    (rectangle 80 15 "solid" "ivory")))
                  (text (string-append "Time: " (format-time (World-counter w))) 24 "black"))
                  (rectangle 300 320 "solid" "ivory")))

;;Changes the color of one circle to the player's color
(: handle-mouse : World Integer Integer Mouse-Event -> World)
(define (handle-mouse w x y e)
  (match e
    ["button-down" (if (World-player-one w)
                       (cond
                     [(and (< x 100) (< y 100) (= (World-r1c1 w) 0))
                      (World (World-x w)
                             (World-y w)
                             1              (World-r2c1 w) (World-r3c1 w)
                             (World-r1c2 w) (World-r2c2 w) (World-r3c2 w)
                             (World-r1c3 w) (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (< x 100) (< 100 y 200) (= (World-r2c1 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) 1              (World-r3c1 w)
                             (World-r1c2 w) (World-r2c2 w) (World-r3c2 w)
                             (World-r1c3 w) (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (< x 100) (> 300 y 200) (= (World-r3c1 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) 1
                             (World-r1c2 w) (World-r2c2 w) (World-r3c2 w)
                             (World-r1c3 w) (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (< 100 x 200) (< y 100) (= (World-r1c2 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) (World-r3c1 w)
                             1              (World-r2c2 w) (World-r3c2 w)
                             (World-r1c3 w) (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (< 100 x 200) (< 100 y 200) (= (World-r2c2 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) (World-r3c1 w)
                             (World-r1c2 w) 1              (World-r3c2 w)
                             (World-r1c3 w) (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (< 100 x 200) (> 300 y 200) (= (World-r3c2 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) (World-r3c1 w)
                             (World-r1c2 w) (World-r2c2 w) 1
                             (World-r1c3 w) (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (> 300 x 200) (< y 100) (= (World-r1c3 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) (World-r3c1 w)
                             (World-r1c2 w) (World-r2c2 w) (World-r3c2 w)
                             1              (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (> 300 x 200) (< 100 y 200) (= (World-r2c3 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) (World-r3c1 w)
                             (World-r1c2 w) (World-r2c2 w) (World-r3c2 w)
                             (World-r1c3 w) 1              (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (> 300 x 200) (> 300 y 200) (= (World-r3c3 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) (World-r3c1 w)
                             (World-r1c2 w) (World-r2c2 w) (World-r3c2 w)
                             (World-r1c3 w) (World-r2c3 w) 1
                             (not (World-player-one w))
                             (World-counter w))]
                     [else w])
                       (cond
                     [(and (< x 100) (< y 100) (= (World-r1c1 w) 0))
                      (World (World-x w)
                             (World-y w)
                             -1             (World-r2c1 w) (World-r3c1 w)
                             (World-r1c2 w) (World-r2c2 w) (World-r3c2 w)
                             (World-r1c3 w) (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (< x 100) (< 100 y 200) (= (World-r2c1 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) -1             (World-r3c1 w)
                             (World-r1c2 w) (World-r2c2 w) (World-r3c2 w)
                             (World-r1c3 w) (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (< x 100) (> 300 y 200) (= (World-r3c1 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) -1
                             (World-r1c2 w) (World-r2c2 w) (World-r3c2 w)
                             (World-r1c3 w) (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (< 100 x 200) (< y 100) (= (World-r1c2 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) (World-r3c1 w)
                             -1             (World-r2c2 w) (World-r3c2 w)
                             (World-r1c3 w) (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (< 100 x 200) (< 100 y 200) (= (World-r2c2 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) (World-r3c1 w)
                             (World-r1c2 w) -1             (World-r3c2 w)
                             (World-r1c3 w) (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (< 100 x 200) (> 300 y 200) (= (World-r3c2 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) (World-r3c1 w)
                             (World-r1c2 w) (World-r2c2 w) -1
                             (World-r1c3 w) (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (> 300 x 200) (< y 100) (= (World-r1c3 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) (World-r3c1 w)
                             (World-r1c2 w) (World-r2c2 w) (World-r3c2 w)
                             -1             (World-r2c3 w) (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (> 300 x 200) (< 100 y 200) (= (World-r2c3 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) (World-r3c1 w)
                             (World-r1c2 w) (World-r2c2 w) (World-r3c2 w)
                             (World-r1c3 w) -1             (World-r3c3 w)
                             (not (World-player-one w))
                             (World-counter w))]
                     [(and (> 300 x 200) (> 300 y 200) (= (World-r3c3 w) 0))
                      (World (World-x w)
                             (World-y w)
                             (World-r1c1 w) (World-r2c1 w) (World-r3c1 w)
                             (World-r1c2 w) (World-r2c2 w) (World-r3c2 w)
                             (World-r1c3 w) (World-r2c3 w) -1
                             (not (World-player-one w))
                             (World-counter w))]
                     [else w]))]
    [else w]))
(check-expect (handle-mouse (World 50 50
                                   0 0 0
                                   0 0 0
                                   0 0 0
                                   #t
                                   0)
                            50 50
                            "button-down")
              (World 50 50
                                   1 0 0
                                   0 0 0
                                   0 0 0
                                   #f
                                   0))
(check-expect (handle-mouse (World 50 50
                                   1 0 0
                                   0 -1 0
                                   -1 0 1
                                   #t
                                   0)
                            50 50
                            "button-down")
              (World 50 50
                                   1 0 0
                                   0 -1 0
                                   -1 0 1
                                   #t
                                   0))

;;Rotates counter-clockwise on [ and clockwise on ]
(: keypress : World String -> World)
(define (keypress w k)
  (match k
    ["[" (World 0 0
                (World-r1c3 w) (World-r1c2 w) (World-r1c1 w)
                (World-r2c3 w) (World-r2c2 w) (World-r2c1 w)
                (World-r3c3 w) (World-r3c2 w) (World-r3c1 w)
                (World-player-one w)
                (World-counter w))]
    ["]" (World 0 0
                (World-r3c1 w) (World-r3c2 w) (World-r3c3 w)
                (World-r2c1 w) (World-r2c2 w) (World-r2c3 w)
                (World-r1c1 w) (World-r1c2 w) (World-r1c3 w)
                (World-player-one w)
                (World-counter w))]
    [else w]))
(check-expect (keypress (World 0 0
                               1 0 -1
                               0 -1 1
                               1 0 -1
                               #t
                               0)
                        "]")
              (World 0 0
                     -1 1 -1
                     0 -1 0
                     1 0 1
                     #t
                     0))

;;Stops program when one player gets three in a row or all circles are
;;occupied
(: win : World -> Boolean)
(define (win w)
  (cond
    [(or (= (World-r1c1 w) (World-r1c2 w) (World-r1c3 w) 1)
         (= (World-r1c1 w) (World-r1c2 w) (World-r1c3 w) -1)
         (= (World-r1c1 w) (World-r2c1 w) (World-r3c1 w) 1)
         (= (World-r1c1 w) (World-r2c1 w) (World-r3c1 w) -1)
         (= (World-r1c1 w) (World-r2c2 w) (World-r3c3 w) 1)
         (= (World-r1c1 w) (World-r2c2 w) (World-r3c3 w) -1)
         (= (World-r2c1 w) (World-r2c2 w) (World-r2c3 w) 1)
         (= (World-r2c1 w) (World-r2c2 w) (World-r2c3 w) -1)
         (= (World-r3c1 w) (World-r3c2 w) (World-r3c3 w) 1)
         (= (World-r3c1 w) (World-r3c2 w) (World-r3c3 w) -1)
         (= (World-r3c1 w) (World-r2c2 w) (World-r1c3 w) 1)
         (= (World-r3c1 w) (World-r2c2 w) (World-r1c3 w) -1)
         (= (World-r1c2 w) (World-r2c2 w) (World-r3c2 w) 1)
         (= (World-r1c2 w) (World-r2c2 w) (World-r3c2 w) -1)
         (= (World-r1c3 w) (World-r2c3 w) (World-r3c3 w) 1)
         (= (World-r1c3 w) (World-r2c3 w) (World-r3c3 w) -1))
     #t]
    [(not (or (= (World-r1c1 w) 0)
              (= (World-r1c2 w) 0)
              (= (World-r1c3 w) 0)
              (= (World-r2c1 w) 0)
              (= (World-r2c2 w) 0)
              (= (World-r2c3 w) 0)
              (= (World-r3c1 w) 0)
              (= (World-r3c2 w) 0)
              (= (World-r3c3 w) 0))) #t]
    [else #f]))
(check-expect (win (World
                    0 0
                    1 -1 -1
                    0 1 0
                    -1 0 1
                    #t
                    0)) #t)
(check-expect (win (World
                    0 0
                    1 -1 -1
                    -1 1 1
                    1 -1 1
                    #t
                    0)) #t)
(check-expect (win (World
                    0 0
                    1 -1 -1
                    -1 -1 1
                    1 0 1
                    #t
                    0)) #f)


;;Timer
(: handle-tick : World -> World)
(define (handle-tick w)
  (match w
    [(World a b
            r1c1 r1c2 r1c3
            r2c1 r2c2 r2c3
            r3c1 r3c2 r3c3
            player
            count)
     (World a b
            r1c1 r1c2 r1c3
            r2c1 r2c2 r2c3
            r3c1 r3c2 r3c3
            player
            (add1 count))]))
(check-expect (handle-tick (World 0 0
                                  0 0 0
                                  0 0 0
                                  0 0 0
                                  #t
                                  0))
              (World 0 0
                     0 0 0
                     0 0 0
                     0 0 0
                     #t 1))


;;Formats time (17 -> 1.7)
(: format-time : Integer -> String)
(define (format-time t)
  (string-append (number->string (quotient t 10))
                 "."
                 (number->string (remainder t 10))))
(check-expect (format-time 17) "1.7")
(check-expect (format-time 1234) "123.4")

(test)
;;Constructs world
(big-bang (World 0 0 0 0 0 0 0 0 0 0 0 #t 0) : World
  [to-draw draw]
  [on-mouse handle-mouse]
  [on-key keypress]
  [stop-when win]
  [on-tick handle-tick 1/10])