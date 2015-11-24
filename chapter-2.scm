
;;; Rational numbers data abstraction implementation (Constructor)

(define (make-rat num den)
  (let ((g (gcd num den)))
    (cond
      ((< den 0) (cons (/ (* -1 num) g) (/ (* -1 den) g)))
      (else (cons (/  num g) (/ den g))))))

;; Operations on the rational numbers data type (Selectors)

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Arthimetic operators on rational numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;; Representing points in the space as data (Constructor)

(define (make-point x y)
  (cons x y))

;; Operations on points (Selectors)

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (square x)
  (* x x))

(define (distance-points p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
           (square (- (y-point p1) (y-point p2))))))

;;; Representing line segments with points (Constructor)

(define (make-segment p1 p2)
  (cons p1 p2))

;; Operations on segments (Selectors)

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
              (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

(define (length-of-segment segment)
  (distance-points (start-segment segment) (end-segment segment)))

;;; Representing rectangles with two perpendicular line segments

(define (make-rectangle s1 s2)
  (cons s1 s2))

(define (length-segment rect)
  (car rect))

(define (height-segment rect)
  (cdr rect))

;; Operations on rectangle

(define (rectangle-area rect)
  (* (length-of-segment (length-segment rect)) (length-of-segment (height-segment rect))))
  
(define (rectangle-perimeter rect)
  (* 2 (+ (length-of-segment (length-segment rect)) (length-of-segment (height-segment rect)))))

;; Define Rectangle

(define p1 (make-point 2 6))
(define p2 (make-point 2 3))
(define p3 (make-point 7 3))

(define s1 (make-segment p1 p2))
(define s2 (make-segment p2 p3))

(define rect (make-rectangle s1 s2))

;;; Defining cons as a procedure (Alternative)

(define (cons/alt x y)
  (lambda (m) (m x y)))

(define (car/alt z)
  (z (lambda (p q) p)))

(define (cdr/alt z)
  (z (lambda (p q) q)))

;;; pair of non-negative numbers as 2^a * 3^b
(define (cons/int a b)
  (* (expt 2 a) (expt 3 b)))

(define (highest-power-of-n a n)
  (if (= (remainder a n) 0)
      (+ 1 (highest-power-of-n (/ a n) n))
      0
      ))

(define (car/int c)
  (highest-power-of-n c 2))

(define (cdr/int c)
  (highest-power-of-n c 3))

;;; Church's numerals

(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; From substitution model

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;;; Defining Interval Arthimetic

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let (
        (p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (if (or (= (upper-bound y) 0) (= (lower-bound y) 0))
                    (display "Error: Interval spans over 0")
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (* -1 (upper-bound y))
                               (* -1 (lower-bound y)))))
                                    



;;; Lists
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))



(define l1 (list 1 2 3 4))

      