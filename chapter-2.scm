;;; Loading all the code in chapter-1 to reuse in this chapter :)

(load "chapter-1.scm")

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

(define (list-ref lst n)
  (if (= n 0)
      (car lst)
      (list-ref (cdr lst) (- n 1))))
    

(define (reverse-list lst)
  (define (loop acc ls)
    (if (null? ls)
	acc
	(loop (cons (car ls) acc) (cdr ls))))
  (loop '() lst))

;; Method to deal with arbitrary number of arguments

(define (same-parity x . y)
  (define (parity? k) (= (remainder k 2) (remainder x 2)))
  (define (loop ls)
    (if (null? ls)
	'()
	(if (parity? (car ls))
	    (cons (car ls) (loop (cdr ls)))
	    (loop (cdr ls))
	)))
  (cons x (loop y)))

;;; Scale list

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor) (scale-list (cdr items) factor))))

;; Map general procedure for list transforms
;;   Scheme has a much more general procedure named map.
;;   which works with arbitrary number of list arguments
(define (map/gen proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map/gen proc (cdr items)))))

;; Scale list in terms of map function

(define (scale-list/map items factor)
  (map/gen (lambda (x) (* x factor)) items))

(define (list-length items)
  (if (null? items)
      0
      (+ 1 (list-length (cdr items)))))

(define (see x)
  (newline)
  (display x))

      
(define (for-each proc items)
  (if (null? items)
      (newline)
      (and (proc (car items)) (for-each proc (cdr items)))))

;;; Trees

(define (count-leaves tree)
  (cond
   ((null? tree) 0)
   ((not (pair? tree)) 1)
   (else
    (+ (count-leaves (car tree))
       (count-leaves (cdr tree))))))

(define (deep-reverse lst)
  (define (loop acc l)
    (if (null? l)
        acc
        (if (list? (car l))
            (loop (cons (deep-reverse (car l)) acc) (cdr l))
            (loop (cons (car l) acc) (cdr l)))))
  (loop '() lst))
  
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(define (fringe lst)
  (cond
   ((null? lst) '())
   ((list? (car lst)) (append (fringe (car lst)) (fringe (cdr lst))))
   (else (cons (car lst) (fringe (cdr lst))))))

;;; Mobile representation problem

;; Each mobile hast a left branch and a right branch,
;; branches are rods of certain length,
;; Each branch has a certain structure which can be a weight or another mobile hanging from that branch.

;; Constructors

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; Getters

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; General procedures on compound Mobile structure

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (pair? struct)
        (total-weight struct)
        struct)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (torque? mobile)
  (define (pseudo-torque? mob)
    (= (* (branch-weight  (left-branch mob)) (branch-length  (left-branch mob)))
       (* (branch-weight  (right-branch mob)) (branch-length  (right-branch mob)))))
  (cond
   ((not (or (pair? (branch-structure (left-branch mobile))) (pair? (branch-structure (right-branch mobile)))))
    (pseudo-torque? mobile))
   ((pair? (branch-structure (left-branch mobile)))
    (and (torque? (branch-structure (left-branch mobile))) (pseudo-torque? mobile)))
   ((pair? (branch-structure (right-branch mobile)))
    (and (torque? (branch-structure (right-branch mobile))) (pseudo-torque? mobile)))
   (else
    (and (torque? (branch-structure (left-branch mobile))) (torque? (branch-structure (right-branch mobile))) (pseudo-torque? mobile)))))
         
   

;; Testing the mobile representation

(define b4 (make-branch 20 50))
(define b3 (make-branch 20 50))
(define m2 (make-mobile b3 b4))
(define b1 (make-branch 20 m2))
(define b2 (make-branch 40 50))
(define m1 (make-mobile b1 b2))

(newline)
(display (total-weight m1))
(newline)
(display (torque? m1))

;;; Mapping over trees

;; Scale Tree

(define (scale-tree tree factor)
  (cond
   ((null? tree) '())
   ((not (pair? tree)) (* tree factor))
   (else (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))))

;; Using map to write Scale Tree

(define (scale-tree/map tree factor)
  (map/gen (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree/map sub-tree factor)
             (* sub-tree factor)))
       tree))


;; Square Tree

(define (square-tree tree)
  (cond
   ((null? tree) '())
   ((not (pair? tree)) (* tree tree))
   (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

;; Using map to write Scale Tree

(define (square-tree/map tree)
  (map/gen (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree/map sub-tree)
             (* sub-tree sub-tree)))
       tree))

;;; Abstracting out a tree-map procedure from the above examples

(define (tree-map proc tree)
  (map/gen (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree))) tree))

;;; Representing sets as lists and defining a subset prcedure on a set,
;;; which gives a list of all subsets of that set

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map/gen (lambda (set)
                            (cons (car s) set))
                          rest)))))

;;; Sequences as conventional interfaces using map, accumulate,
;;; enumerate and filter as independent modules

(define (filter predicate sequence)
  (cond
   ((null? sequence) '())
   ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
   (else (filter predicate (cdr sequence)))))
      
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-trees tree)
  (fringe tree))

;;; We use the above pluggable modules to compose future programs

(define (sum-odd-squares tree)
  (accumulate + 0
              (map/gen square
                   (filter odd?
                           (enumerate-trees tree)))))

(define (fib n)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (even-fibs n)
  (accumulate cons '()
              (filter even?
                      (map/gen fib
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons '()
              (map/gen square
                   (map/gen fib
                        (enumerate-interval 0 n)))))


;; Exercise 2.33

(define (map/acc proc sequence)
  (accumulate (lambda (x y) (cons (proc x) y))  '() sequence))

(define (append/acc seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(define (count-leaves/acc tree)
  (accumulate (lambda (x y) (+ 1 y)) 0
              (enumerate-trees tree)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (accumulate (lambda (x y) (cons (car x) y)) '() seqs))
            (accumulate-n op init (accumulate (lambda (x y) (cons (cdr x) y)) '() seqs)))))

;;; Vector = (1 2 3) , matrix = ((1 2 3) (4 5 6) (7 8 9))

;; Dot Product of two vectors

(define (dot-product v w)
  (accumulate + 0
              (map * v w)))

;;  Matrix * Vector

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v))  m))

;; Transpose Matrix

(define (transpose mat)
  (accumulate-n cons '() mat)) 

;; Matrix Multiplication

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (matrix-*-vector cols x))
         m)))
  

;; Accumulate is also called a fold-right and there is also a fold-left (I used a similar method for reverse-list)

(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial seq))

(define (reverse/fl seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))

(define (reverse/acc seq)
  (accumulate (lambda (x y) (append y (list x))) '() seq))

;;; Nested Mappings

(define (per n)
  (accumulate append '()
              (map (lambda (i)
                     (map (lambda (j)
                            (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
                     
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
