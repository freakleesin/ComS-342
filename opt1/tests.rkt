#lang racket
(provide (all-defined-out))


(define r0'(
            (10 13) ; 3
            (7 8) ; 1
            (1 5); 4
            (3 10); 7
            (5 7); 2
            (0 10); 10
            (4 13); 9
            (8 14))); 6

(define r1 '(
             (2 5) ; 3
             (0 6) ; 6
             (4 11) ; 7
             (5 9) ; 4
             (1 3) ; 2
             (3 8) ; 5
             (6 10) ; 4
             (8 21) ; 3
             ))


(define r2 '(
             (0 19); 19 
             (1 6); 5
             (7 13); 6 
             (4 8); 4
             (2 11); 9
             (3 10); 7
             ))



(define e1 '(lambda x ((lambda y y) z)))
; (lambda x z)

(define e2 '(lambda x ((lambda x y) z)))
; (lambda x y)

(define e3 '((lambda x (lambda x x))  y))
; (lambda x x)


(define e4 '(((lambda x (lambda y (x y))) z1) z2))
; (z1 z2)

(define e5 '(((lambda x (lambda y (x y))) (lambda w (w p))) (lambda z (z z))))
; (p p)

(define zero '(lambda g (lambda y y)))
(define succ '(lambda n (lambda f (lambda x (f ((n f) x))))))

(define succ1 '(lambda n1 (lambda f1 (lambda x1 (f1 ((n1 f1) x1))))))
(define add (list 'lambda 'a1
                  (list 'lambda 'b1
                        (list
                         (list 'a1 succ1) 'b1))))
(define mul (list 'lambda 'a2
                  (list 'lambda 'b2
                        (list
                         (list 'a2
                               (list add 'b2))
                         zero))))

(define e6 (list succ zero))
; '(lambda f (lambda x (f x)))

(define e7 (list (list add (list succ zero)) (list succ (list succ zero))))
; '(lambda f1 (lambda x1 (f1 (f1 (f1 x1)))))

(define e8 (list (list mul (list succ (list succ zero))) (list succ (list succ (list succ zero)))))
; '(lambda f1 (lambda x1 (f1 (f1 (f1 (f1 (f1 (f1 x1))))))))
