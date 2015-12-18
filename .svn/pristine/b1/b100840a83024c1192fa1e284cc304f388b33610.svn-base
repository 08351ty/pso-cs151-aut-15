#lang typed/racket
(require "../include/uchicago151.rkt")
(require typed/test-engine/racket-tests)
(require typed/2htdp/image)
(require typed/2htdp/universe)

;; Grader: Good work!

;;Defines structure world that takes counter and number of clicks
(define-struct World
  ([counter : Integer]
   [click : Integer]))

;;draws number of clicks and time elapsed
(: draw : World -> Image)
(define (draw w)
  (overlay (above (text (string-append "Count: " (number->string (World-click w)))
                         24
                         "black")
                  (rectangle 400 20 "solid" "ivory")
            (text (string-append "Time: " (format-time (World-counter w)))
                    16
                    "black"))
           (rectangle 400 200 "solid" "ivory")))

;;Creates timer
(: handle-tick : World -> World)
(define (handle-tick w)
  (match w
    [(World c d) (World (add1 c) (World-click w))]))
(check-expect (handle-tick (World 0 0)) (World 1 0))
(check-expect (handle-tick (World 120 0)) (World 121 0))

;;Listens to mouse click, adding one to clicks in world structure
(: handle-mouse : World Integer Integer Mouse-Event -> World)
(define (handle-mouse w x y e)
  (match e
    ["button-down" (World (World-counter w) (+ (World-click w) 1))]
    [_ w]))
(check-expect (handle-mouse (World 0 0) 120 120 "button-down") (World 0 1))
(check-expect (handle-mouse (World 0 0) 120 120 "button-up") (World 0 0))

;;Stops program when user clicks 20 times
(: twenty-clicks : World -> Boolean)
(define (twenty-clicks w)
  (match w
    [(World c d) (if (= (World-click w) 20)
                     #t
                     #f)]))
(check-expect (twenty-clicks (World 0 0)) #f)
(check-expect (twenty-clicks (World 0 20)) #t)

(: format-time : Integer -> String)
;; format 17 -> 1.7
(define (format-time t)
  (string-append (number->string (quotient t 10))
                 "."
                 (number->string (remainder t 10))))
(check-expect (format-time 0) "0.0")
(check-expect (format-time 17) "1.7")
(check-expect (format-time 123) "12.3")

;; Grader: Good helper function. You can also use real->decimal-string.

(test)

(big-bang (World 0 0) : World
          [to-draw draw]
          [on-tick handle-tick 1/10]
          [on-mouse handle-mouse]
          [stop-when twenty-clicks])

