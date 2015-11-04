;Procedurural Abstraction
(define (square x) (* x x))

;Data Abstraction
(define a (* 5 5))
(define b (+ a (* 5 a)))

;Actual Procedurural Abstraction
(define square (lambda (x) (* x x)))

;Average x y -> (x+y)/2
(define (average x y)
  (/ (+ x y) 2))

;Mean Square x y -> (x^2 + y^2)/2
(define (mean-square x y)
  (average (square x) (square y)))

;abs x = {x if (x > 0) | -x if (x < 0) | 0 if (x = 0) }
(define (abcisa x)
  (cond ((< x 0) (- x))
        ((= x 0) 0)
        ((> x 0) x)))

;abs x -> syntactic sugar "if" in the place of cond
(define (abcisa x)
  (if (< x 0)
      (- x)
      x))

;new-if

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))


;Sqrt x -> square root of x
;By packaging the functions inside a bigger function,
;the function names are internal to this procedure only and some other programmer
;also can use the same names again to define other functions.

(define (square-root x)
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) 0.0001))
  (try 1))

; x y -> (x^2 + y^2)
(define (sos x y)
  (+ (square x) (square y)))

; a b c -> min(a b c) -> c -> (a^2 + b^2)
(define (max-sos x y z)
  (sos (max x y) (max (min x y) z)))

; a b -> a + |b|
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; x -> (x)^(1/3)
(define (cube-root x)
  (define (try guess)
    (if (good-enough? guess)
        (improve guess)
        (try (improve guess))))
  (define (improve guess)
    (/ (+ (* 2 guess) (/ x (square guess))) 3))
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) 0.0001))
  (try 1))

; n! -> n*(n-1)!
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; n! -> 1*2*...*n
(define (factorial/iter n)
  (define (fact-iter product counter)
    (if (> counter n)
        product
        (fact-iter (* counter product) (+ counter 1))))
  (fact-iter 1 1))

; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; Fibonacci Number (Tree Recursive Process)
(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

; Fibonacci Number (Iterative Process)
(define (fib/iter n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

; f(n) = (if (< n 3) n (+ f(n-1) (* 2 f(n-1)) (* 3 f(n-1))))
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

; f(n) iterative
(define (f/iter n)
  (define (f-iter a b c count)
    (cond
      ((= count 0) c)
      ((= count 1) b)
      ((= count 2) a)
      (else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (f-iter 2 1 0 n))

; Pascal's Triangle P(row, column) -> P(row-1, column) + P(row-1, column-1)
(define (pascal row column)
  (cond
    ((= column 1) 1)
    ((= column row) 1)
    ((> column row) "Invalid Entry")
    (else (+ (pascal (- row 1) column) (pascal (- row 1) (- column 1))))))

; Exponentiation(Recursive)
(define (exponent b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; Exponentiation(Iterative)
(define (expt/iter b n)
  (define (expt-iter product counter)
    (if (> counter n)
        product
        (expt-iter (* product b) (+ counter 1))))
  (expt-iter 1 1))

; Fast Exponentiation(Recursive)
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
                   
; Fast Exponentiation(Iterative)
(define (fast-expt/iter b n)
  (define (fast-expt-iter product power count)
    (cond
      ((= count 0) product)
      ((even? count) (fast-expt-iter product (square power) (/ count 2)))
      ((odd? count) (fast-expt-iter (* product power) (square power) (/ (- count 1) 2)))))

  (fast-expt-iter 1 b n))

; Fast Multiplication
(define (fast-mul a b)
  (cond
    ((= b 1) a)
    ((even? b) (* 2 (fast-mul a (/ b 2))))
    ((odd? b) (+ a (fast-mul a  (- b 1))))))

; Fast Multiplication(Iterative)
(define (fast-mul/iter a b)
  (define (fast-mul-iter result x y)
    (cond
      ((= y 0) result)
      ((even? y) (fast-mul-iter result (* 2 x) (/ y 2)))
      ((odd? y) (fast-mul-iter (+ result x) (* 2 x) (/ (- y 1) 2)))))
  (fast-mul-iter 0 a b))


; Greatest Common Divisor(Euclid's Algorithm)
(define (gcd/euler a b)
  (if (= b 0)
      a
      (gcd/euler b (remainder a b))))
