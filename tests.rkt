#lang racket

(require "main.rkt"
         )

(define (test-input size)
  (for/fold ([ls '($)]) ([i (in-range (* 2 size))])
    (if (< i size)
        (cons 'b ls)
        (if (= i size)
            (cons 'a ls)
            (list* 'a 'c ls)))))

(define (test size)
  (printf "Size: ~a\n" size)
  (let ((in (test-input size)))
    (time (parse in))))

;; command line racket, 3gbz core i7, 8gb 1600mhz ddr3 ram
(test 1000)
(test 10000)
;; 0.36 seconds
(test 100000)
;; 3.1 seconds
(test 1000000)
;; 32 seconds
(test 10000000)
;; ? seconds, probably ~300
;(test 10000000)
