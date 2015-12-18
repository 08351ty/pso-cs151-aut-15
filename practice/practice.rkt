#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/uchicago151.rkt")

(define-type (RLE a)
  (Listof (Run a)))

(define-struct (Run a)
  ([count : Integer]
   [value : a]))

(: rle-length : All (a) (RLE a) -> Integer)
(define (rle-length list)
  (cond
    [(empty? list) 0]
    [(cons? list) (+ (Run-count (first list))
                     (rle-length (rest list)))]))
