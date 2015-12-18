#lang typed/racket
(require typed/2htdp/image)
(require typed/test-engine/racket-tests)

(: bbt (-> Integer Image-Color Image))
;; bbt means "black-backed triangle"
;; The resulting image is an nxn square.
(define (bbt n c)
  (if (< n 0)
      (error "negative n")
      (overlay (triangle (quotient (* 2 n) 3) "solid" c)
               (square n "solid" "black"))))

(: gbc (-> Integer Image-Color Image))
;; gbc means "gray-backed circle"
;; The resulting image is an nxn square.
(define (gbc n c)
  (if (< n 0)
      (error "negative n")
      (overlay (circle (quotient n 3) "solid" c)
               (square n "solid" "darkgray"))))

(: design (-> Integer Image))
;; Draws an asymmetrical design in an nxn square.
(define (design n)
  (cond
    [(< n 0) (error "negative")]
    [(= n 1) (square 1 "solid" "black")]
    [else 
     (frame (above (beside (gbc (quotient n 2) "black")
                           (bbt (quotient n 2) "ivory"))
                   (beside (gbc (quotient n 2) "red")
                           (gbc (quotient n 2) "darkred"))))]))

(: quarter (-> Image Image))
;;Produces an image which is a quarter the area of the original image
(define (quarter img)
  (scale 1/4 img))

(: row (-> Integer Image Image))
;;Produces a row of n images
(define (row x img)
  (cond
    [(<= x 0) empty-image]
    [else (beside (row (- x 1) img) img)]))

(: col (-> Integer Image Image))
;;Produces a column of n images
(define (col x img)
  (cond
    [(<= x 0) img]
    [else (above (col (- x 1) img) img)]))

(: grid (-> Integer Integer Image Image))
;;Produces a grid of images. First arg is width, second arg is height
(define (grid x y img)
  (col y (row x img)))

(: shrinking-row (-> Image Image))
;;Produces row of images where each subsequent image is 3/4 size of previous
(define (shrinking-row img)
  (cond
    [(<= (image-height img) 1) empty-image]
    [else (beside/align "bottom" img (shrinking-row (scale 3/4 img)))]))

(: shrinking-row-scaled (-> Image Image))
;;Produces row of images where each subsequent image is 3/4 size of previous
(define (shrinking-row-scaled img)
  (cond
    [(<= (image-height img) 1) empty-image]
    [else (beside/align "bottom" (scale 3/4 img)
                        (shrinking-row (scale 3/4 (scale 3/4 img))))]))

(: bsr (-> Image Image))
;;Bidirectional shrinking row
(define (bsr img)
;;  (beside/align "top" (flip-vertical (flip-horizontal (shrinking-row-scaled img)))
;;                (shrinking-row img)))
  (beside/align "top" (flip-vertical (flip-horizontal (shrinking-row (scale 3/4 img)))) (shrinking-row img)))

(: shrink-row (-> Image Image))
;;Constructs a row shrinking by half the size
(define (shrink-row img)
  (cond
    [(< (image-height img) 1) empty-image]
    [else (beside (scale 1/2 img)
                  (above (shrink-row (scale 1/2 img))
                         (shrink-row (scale 1/2 img))))]))

(: shrink-col (-> Image Image))
;;Constructs a column shrinking by half the size
(define (shrink-col img)
  (cond
    [(< (image-height img) 1) empty-image]
    [else (above (beside (shrink-col (scale 1/2 img))
                         (shrink-col (scale 1/2 img)))
                 (scale 1/2 img))]))

(: shrink-grid (-> Image Image))
;;Creates a grid shrinking by half the size
(define (shrink-grid img)
  (cond
    [(< (image-height img) 1) empty-image]
    [else 
  (above
   (beside (beside (shrink-col img)
                   (shrink-col img))
           (shrink-grid (scale 1/2 img)))
   (beside img (above (shrink-row img)
                      (shrink-row img))))]))

(: main (-> Image Image))
;;Produces figure
(define (main img)
  (cond
    [(= (image-width img) 1) (above (beside img img) (beside img img))]
    [else
  (above (beside (flip-horizontal (shrink-grid img)) (shrink-grid img))
         (flip-vertical (beside (flip-horizontal (shrink-grid img))
                                (shrink-grid img))))]))


"====== grader's tests ======="
(require typed/test-engine/racket-tests)

(define d8 (design 8))
(define d16 (design 16))
(define d32 (design 32))
(define d64 (design 64))

">>>>> quarter <<<<<"

(check-expect (image-width (quarter d8))  4)
(check-expect (image-height (quarter d8))  4)
(check-expect (image-width (quarter d64))  32)
(check-expect (image-height (quarter d64))  32)

"before:"
(design 64)
"after:"
(quarter (design 64))

">>>>> row <<<<<"
(check-expect (image-width (row 3 d8)) 24)
(check-expect (image-height (row 3 d8)) 8)
(check-expect (image-width (row 5 d16)) 80)
(check-expect (image-height (row 5 d16)) 16)
"(row 3 d32)"
(row 3 d32)
"(row 5 d64)"
(row 5 d64)

">>>>> col <<<<<"
(check-expect (image-width (col 3 d8)) 8)
(check-expect (image-height (col 3 d8)) 24)
(check-expect (image-width (col 5 d16)) 16)
(check-expect (image-height (col 5 d16)) 80)
(beside (col 3 d32) (col 5 d64))

">>>>> grid <<<<<"
(check-expect (image-width (grid 5 5 d16)) 80)
(check-expect (image-height (grid 5 5 d16)) 80)
(check-expect (image-width (grid 3 3 d8)) 24)
(check-expect (image-height (grid 3 3 d8)) 24)

(grid 6 6 d16)
(grid 3 3 d32)


">>>>> shrinking-row <<<<<"
(check-expect (image-height (shrinking-row d32)) 32)
(check-expect (image-height (shrinking-row d16)) 16)
(shrinking-row d32)
(shrinking-row d64)

">>>>>bsr<<<<<"
(check-expect (image-height (bsr d32)) 32)
(check-expect (image-height (bsr d8)) 8)
(bsr d32)
(bsr d64)

">>>>> main <<<<<"
(define m8 (main d8))
(define m16 (main d16))
(define m32 (main d32))

(check-expect (image-width m8) 32)
(check-expect (image-height m8) 32)
(check-expect (image-width m16) 64)
(check-expect (image-height m16) 64)
(check-expect (image-width m32) 128)
(check-expect (image-height m32) 128)
"(main d16)"
m16
"(main d32)"
m32

(test)


;; === correctness ===

;; quarter         2/  3

;; row             4/  4
;; col             4/  4
;; grid            5/  5

;; shrinking-row   6/  6
;; bsr             6/  6

;; main            20/ 20

;; === style ===

;; code layout                        8/  8
;; identifiers are well named         8/  8
;; program decomposition (helpers)    8/  8

;; contracts (type ascriptions)       8/  8
;; well-written purposes              8/  8

;; adequate tests, N/A this time

;; clarity (clear logic)              8/  8

;; svn used correctly                 4/  4

;; _total-score_                     99/ 100