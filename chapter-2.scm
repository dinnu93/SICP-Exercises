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



