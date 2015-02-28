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
  (append (list f) (build-infinite-list f)))


(define (no-smaller-divisors div number)
  (cond
    [(= div 1) true]
    [else (cond
            [(= div 0) false]
            [(= (remainder number div) 0) false]
            [else (no-smaller-divisors (- div 1) number)])]))

(test (no-smaller-divisors 2 20) #f); 
(test (no-smaller-divisors 6 7) #t);

;(prime? n) → boolean?
;  n : exact-positive-integer?
;Returns true if n is prime.
(define (prime? n)
  (if (<= n 1) 
      #f
      (not (ormap (lambda (x) (exact-positive-integer? (/ n x)))
                  (take-while (lambda (y) (< y n))
                              (larger-positive-integers 2))))))
  

(define (larger-positive-integers n)
  (append (list n) (larger-positive-integers (+ n 1))))

;primes : (listof exact-positive-integer?)
;The list of all primes.
(define primes (filter prime? (larger-positive-integers 1)))


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
(test (lcs-length "artist" "artsy") 4)
(test (lcs-length "arts" "artsy") 4)
(test (lcs-length "32905204" "3940205") 5)
(test (lcs-length "" "a") 0)
(test (lcs-length "" "") 0)
(test (lcs-length "asdfgh" "qwert") 0)
(test (lcs-length "asdfgh" "hjkl") 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST BUILD-TABLE"
(test (- 4 3)
      (vector-ref (vector-ref (build-table 5 5 (lambda (x y) (- x y))) 4 )3))

(test (get-table-element (build-table 3 3 +) 2 2) 4)
(test (get-table-element (build-table 3 3 +) 1 2) 3)
(test (get-table-element (build-table 12 12 *) 8 11) 88)
(test (get-table-element (build-table 13 13 *) 7 12) 84)
(test (get-table-element (build-table 5 5 -) 3 2) 1)
(test (get-table-element (build-table 5 5 /) 0 3) 0)

(test (get-table-element 
       (build-table 10 10 
                    (lambda (x y) 
                      (or (even? x) (even? y)))) 5 7) #f)

(test (get-table-element
       (build-table 10 10
                    (lambda (x y)
                      (even? x))) 4 7) #t)

(test (get-table-element
       (build-table 3 3
                    (lambda (x y)
                      #f)) 1 2) #f)

(test (get-table-element
       (build-table 5 5
                    (lambda (x y)
                      (cond [(= x 4) "Good"]
                            [(= y 3) "Okay"]
                            [else "Great"]))) 3 3)
      "Okay")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST TAKE_WHILE"
(test (take-while odd? (list 1 3 4))(list 1 3))
(test (take-while odd? empty) empty)
(test (take-while odd? (list 1)) (list 1))
(test (take-while even? (list 1 3 4)) empty)
(test (take-while even? (list 2 4)) (list 2 4))

(test (take-while (lambda (n) (< n 5)) (list 1 2 3 4 5 1 2)) 
      (list 1 2 3 4))

(test (take-while (lambda (x) (> x 1))
                  (list 2 3 1 5 4))
                  (list 2 3))

(test (take-while (lambda (x) (< x 4)) 
                  (list 5 6 7))
      empty)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST BUILD_INFINITE_LIST"
(define (f i) (+ i 1))
(test (list-ref (build-infinite-list 4) 6) 4)
(test (list-ref (build-infinite-list 3) 1) 3)
(test (list-ref (build-infinite-list 5) 10) 5)
(test (list-ref (build-infinite-list 0) 0) 0)
(test (list-ref (build-infinite-list f) 10) 0)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST PRIME?"
(test (prime? -1) #f)
(test (prime? 0) #f)
(test (prime? 1) #f)
(test (prime? 2) #t)
(test (prime? 3) #t)
(test (prime? 4) #f)
(test (prime? 6) #f)
(test (prime? 7) #t)
(test (prime? 8) #f)
(test (prime? 6708) #f)
(test (prime? 6719) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST PRIMES"
(test (first primes) 2)
(test (second primes) 3)
(test (third primes) 5)
(test (fourth primes) 7)
(test (fifth primes) 11)
(test (sixth primes) 13)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST PRIME/FAST?"
(test (prime?/fast 0) #f)
(test (prime?/fast 1) #t)
(test (prime?/fast 2) #t)
(test (prime?/fast 3) #t) ;Very Slow....
(test (prime?/fast 4) #f)
(test (prime?/fast 6) #f)
(test (prime?/fast 7) #t) ;Broken
(test (prime?/fast 8) #f)
(test (prime?/fast 6708) #f)
(test (prime?/fast 6719) #t)  ;Broken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"TEST PRIMES/FAST"
;(test (primes/fast 87) #f)
;(test (primes/fast 89) #t)
;(test (primes/fast 459) #f)
;(test (primes/fast 1000) #f)
;(test (primes/fast 1002) #f)
;(test (primes/fast 1011) #f)
;(test (primes/fast 1005) #f)
;(test (primes/fast 3017) #f)
;(test (primes/fast 1513) #f)
