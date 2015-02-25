#lang lazy


(define print-only-errors #t)

;test function, passes if the parameters l r are equal, otherwise fails
(define (test l r)
  (if (equal? l r)
      (if print-only-errors
          ()
          (printf "Test Passed~n"))
      (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))

(printf "hello world")



