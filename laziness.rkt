#lang lazy

(define print-only-errors #f)

;;test function, passes if the parameters l r are equal, otherwise fails
(define (test l r)
  (if (equal? l r)
      (if print-only-errors
          ()
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


;;(build-infinite-list f) → (listof any/c)
;;  f : (exact-nonnegative-integer? . -> . any/c)
;; Lazily constructs the infinite list such that (list-ref (build-infinite-list f) i) returns (f i).
(define (build-infinite-list f)
  (cons f (build-infinite-list (+ f 1))))


(define (no-smaller-divisors div number)
  (cond
    [(= div 1) true]
    [else (cond
            [(= (remainder number div) 0) false]
            [else (no-smaller-divisors (- div 1) number)])]))

(test (no-smaller-divisors 2 20) #f); 
(test (no-smaller-divisors 6 7) #t);

;(prime? n) → boolean?
;  n : exact-positive-integer?
;Returns true if n is prime.
(define (prime? n)
  (no-smaller-divisors n (floor (sqrt n))))
  

;primes : (listof exact-positive-integer?)
;The list of all primes.
(define primes (filter prime? (build-infinite-list 1)))


(define (no-smaller-prime-divisors index num)
  (cond
    [(< 
      (floor (sqrt num)) 
      (list-ref primes index)) true]
    [else (cond
            [(= (remainder num (list-ref primes index)) 0) false]
            [else (no-smaller-divisors (- index 1) num)])]))

;(prime?/fast n) → boolean
;  n : exact-positive-integer?
;Returns true if n is prime, but tests only prime factors from primes/fast.
(define (prime?/fast n)
  (no-smaller-prime-divisors n (floor (sqrt n))))


;(build-table rows cols f) → (vectorof (vectorof any/c))
;  rows : exact-positive-integer?
;  cols : exact-positive-integer?
;     f : (exact-nonnegative-integer? exact-nonnegative-integer? . -> . any/c)
(define (build-table rows cols f)
  (build-vector cols (lambda (x)
                       (build-vector rows (lambda (y)
                                            (f x y))))))

;get's the nth char of a string starting with 1
(define (get-nth-char s n)
  (if (equal? n 0)
      #f
      (string-ref s (- n 1))))

;retrieves an element from a table
(define (get-table-element table x y)
  (vector-ref (vector-ref table x )y))

;Computes the length of the longest common subsequence of two strings s1 and s2.
(define (lcs-length s1 s2)
  (letrec ([table (build-table (+ 1 (string-length s1))
               (+ 1 (string-length s2))
               (lambda (x y)
                 (cond 
                   [(or (equal? 0 x) (equal? 0 y)) 0]
                   [(equal? (get-nth-char s1 y) (get-nth-char s2 x)) (+ 1 (get-table-element table (- x 1) (- y 1)))]
                   [else (max   (get-table-element table x (- y 1))   (get-table-element table (- x 1) y)    )]
                 )))])
    (get-table-element table (string-length s2) (string-length s1) )
  ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST LCS-LENGTH"
(test (lcs-length "meo" "meow") 3)
(test (lcs-length "aaabbb" "bbbaaa") 3)
(test (lcs-length "ababqb" "bbbaaa") 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST BUILD-TABLE"
(test (- 4 3)
      (vector-ref (vector-ref (build-table 5 5 (lambda (x y) (- x y))) 4 )3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST TAKE_WHILE"
(test (take-while odd? (list 1 3 4))
      (list 1 3))

(test (take-while (lambda (n) (< n 5)) (list 1 2 3 4 5 1 2)) 
      (list 1 2 3 4))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST BUILD_INFINITE_LIST"
(test (list-ref (build-infinite-list 4) 6) 10)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST PRIME?"
;(test (prime? 0) #f)
(test (prime? 1) #t)
(test (prime? 2) #t)
(test (prime? 3) #t)
(test (prime? 4) #f)
(test (prime? 6) #f)
(test (prime? 7) #t) ;Broken
(test (prime? 8) #f)
(test (prime? 6708) #f)
(test (prime? 6719) #t)  ;Broken

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST PRIMES"
(test (list-ref primes 0) 1)
(test (list-ref primes 1) 2)
(test (list-ref primes 2) 3)
;(test (list-ref primes 3) 5) ;Very Slow....

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST PRIME/FAST?"
;(test (prime?/fast 0) #f)
(test (prime?/fast 1) #t)
(test (prime?/fast 2) #t)
(test (prime?/fast 3) #t) ;Very Slow....
(test (prime?/fast 4) #f)
(test (prime?/fast 6) #f)
(test (prime?/fast 7) #t) ;Broken
(test (prime?/fast 8) #f)
(test (prime?/fast 6708) #f)
(test (prime?/fast 6719) #t)  ;Broken
