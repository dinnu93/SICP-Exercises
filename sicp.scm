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

; Smallest Divisor - O(sqrt(N))
(define (smallest-divisor n)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((zero? (remainder n test-divisor)) test-divisor)
          ((= test-divisor 2) (find-divisor 3))
          (else (find-divisor (+ test-divisor 2)))))
  (find-divisor 2))
    
; Prime? if (smallest-divisor n) = n
(define (prime? n)
  (if (= n 1)
      #f
      (= (smallest-divisor n) n)))

; a^p(mod p)
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

; Fermat's primality test
; Lookout for carmichael numbers (very rare) which are anomalies for Fermat's test
(define (prime/fermat? n)
  (cond
    ((= n 2) #t)
    (else (= (expmod 2 n n) 2))))


;; Cube
(define cube (lambda (x) (* x x x)))
(define identity (lambda (x) x))
;; Number sum
(define (sum-integer a b)
  (sum-gen a inc b identity))

;; Square sum
(define (sum-square a b)
  (sum-gen a inc b square))

;; 1/n*(n+2) Sum
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum-gen a pi-next b pi-term)) 

;; Increment by 1
(define (inc n) (+ 1 n))

;; General Sum
(define (sum-gen a next b term)
  (if (> a b)
      0
      (+ (term a)
         (sum-gen (next a) next b term))))

;; General Sum (Iteration)
(define (sum-gen/iter a next b term)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; Integral
(define (integral f a b dx)
  (* (sum-gen (+ a (/ dx 2.0)) (lambda (x) (+ x dx)) b f) dx))


;; Simpson's Integration
(define (integral/simpson f a b n)
  (define h (/ (- b a) n))
  (define (next x) (+ x (* 2 h)))
  (* (+ (f a) (* 2 (sum-gen (+ a (* 2 h)) next (- b (* 2 h)) f)) (* 4 (sum-gen (+ a h) next (- b h) f)) (f b)) (/ h 3.0)))

;; Wallis Product : (n =: 1->infinity) : (2n/(2n-1))*(2n/(2n+1)) = pi/2
(define (wallis n)
  (* (/ (* 2 n) (- (* 2 n) 1)) (/ (* 2 n) (+ (* 2 n) 1))))

(define (wallis-product a b)
  (product-gen a inc b wallis))

;; General Product
(define (product-gen a next b term)
  (if (> a b)
      1
      (* (term a)
         (product-gen (next a) next b term))))


;; General Product (Iteration)
(define (product-gen/iter a next b term)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; Accumulate : Higher order procedure for sum and product
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;; Accumulate/Iteration : Higher order procedure for sum and product
(define (accumulate/iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; Filtered Accumulate : Higher order procedure for accumulate
(define (filtered-accumulate filter? combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter? a) (term a) null-value)
                (filtered-accumulate filter? combiner null-value term (next a) next b))))

;; sum of prime numbers between [a,b]
(define (sum-of-primes a b)
  (filtered-accumulate prime? + 0 identity a inc b))

;; product of prime numbers between [a,b]
(define (product-of-primes n)
  (filtered-accumulate prime? * 1 identity 1 inc n))

;; product of numbers below n with G.C.D(x,n) = 1
(define (product-of-coprimes n)
  (filtered-accumulate (lambda (x) (= (gcd/euler x n) 1)) * 1 identity 1 inc n))
 

(define (unity x) 1)

;; Observation from prime-avg is that on an average as 'n'
;; becomes large prime-avg tends to move towards normal average.
(define (prime-sum n)
  (filtered-accumulate prime? + 0 square 1 inc n))

(define (sine x)
  (define (term n)
    (/ (* (exponent -1 (+ n 1)) (exponent x (- (* 2 n) 1))) (factorial (- (* 2 n) 1))))
  (accumulate + 0 term 1 inc 10))

;; pi = 3.1416...
(define pi (* 2 (wallis-product 1 2000)))

;; for-each
(define (show-list lst)
  (for-each display lst))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt/fixed x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))



(define (phi start) (fixed-point (lambda (x) (+ 1 (/ 1 x))) start))

;; x**x = 1000 soln.
(define (hyper-power start)
  (fixed-point (lambda (x) (* 3 (/ (log 10) (log x)))) start))

;; Continued-Fractions
(define (cont-frac n d k)
  (define (cont-frac/aux i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i) (+ (d i) (cont-frac/aux (+ i 1))))))
  (cont-frac/aux 1))
        
  
;; Continued-Fractions/iter
(define (cont-frac/iter n d k)
  (define (iter i Num Den)
    (if (= i 1)
        (/ Num Den)
        (iter (- i 1) (n (- i 1)) (+ (d (- i 1)) (/ Num Den)))))
  (iter k (n k) (d k)))

(define (aux x)
  (let ((r (remainder x 3)))
    (if (or (= r 0) (= r 1))
        1
        (* 2 (+ 1 (quotient x 3))))))
   
(define e (+ 2 (cont-frac/iter unity aux 20)))

(define (tan-cf x k)
  (cont-frac/iter   (lambda (n) (if (= n 1) x (* -1 (square x)))) (lambda (x) (- (* 2 x) 1)) k)
  )

;; Procedures Returning Procedures

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt/damp x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
  
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

;; Derivative of a function
(define (deriv g)
  (define dx 0.0001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

;; Newton-Raphson method : g is any function
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt/newton x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

;; General procedure from abstracting all out
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-general-average-damp x)
    (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt-general-newton-transform x)
    (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))

;; cubic equation solution x**3 + a*x**2 + b*x + c
(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

(define (cubic-root a b c)
  (newtons-method (cubic a b c) 1.0))

;; double
(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (smooth f)
  (define dx 0.0001)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))
  
(define (nth-root/gen x n)
  (fixed-point ((repeated average-damp (floor (/ (log x) (log 2)))) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  try)

(define (sqrt/iter-improve x)
  (define (good-enough? guess)
    (< (abs (- guess (improve guess))) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))