#lang lazy


(define print-only-errors #t)

;test function, passes if the parameters l r are equal, otherwise fails
(define (test l r)
  (if (equal? l r)
      (if print-only-errors
          (printf "meh")
          (printf "Test Passed~n"))
      (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))


;; returns the prefix of l such that for all elements p returns true
(define (take-while p l)
   (if (empty? l)
       empty
       (if (p (first l))
           (cons (first l) (take-while p (rest l)))
           empty
           )))
   

(display (take-while (lambda (n) (< n 5)) (list 1 2 3 4 5 1 2)))


