#lang racket
(provide (all-defined-out))

;(define progs '(p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11))


;; syntax error
(define p0 'x)

(define p1 '(decl x))

(define p2 '((decl x) x))

(define p3 '((decl 1)))

(define p4 '((decl x y)))

(define p5 '((assign x y 1) (decl y)))

(define p6 '((assign 10 x) (decl y)))

(define p7 '((assign x 10) (decl z) (if (gt 1 2) (decl x))))

(define p8 '((assign x 10) (decl z) (if (gt 1 2) ((decl x) x))))

(define p9 '((assign x 10) (decl z) (if (gt 1 2) x)))

(define p10 '((assign x 10) (decl z) (if (gt 1 2) ((assign x 10) (decl z) (if (gt 1 2) (decl x))))))

(define p11 '())

(define p12 '(()))

;; no syntax error 
(define p13 '(
              (assign x 10)
              (if (and (gt x y)
                       (lt z w))
                  (
                   (assign y 10)
                   (if (or (gt x y)
                           (not (lt y z)))
                       (
                        (decl z)
                       ))
                   (assign z y)
                   (if (gt 1 2)
                       (
                        (decl z)
                        ))
                   ))))


;; for semantics 
(define q0 '((decl x) (assign x y)))  ;; ((x 1) (y 1)) for ((y 1))

(define q1 '((decl x) (assign x (+ x 1)))) ;; ((x 1))

(define q2 '((decl x) (decl y) (assign x (+ y 1)) (assign y (+ x 1)))) ;; ((y 2) (x 1))

(define q3 '((decl x) (assign x y) (if (gt x 3)
                                       ((assign x 12)))
                      (assign x y))) ;; ((x 0) (y 0))  or ((x 10) (y 10))

(define q4 '((decl x)
             (assign x y)
             (if (gt x 3)
                 (
                  (decl z)
                  (assign z (* x y))
                  )
                 )
             )) ;;  ((x 1) (y 1)) for ((y 1)) ((x 4) (y 4)) for ((y 4))

(define q5 '((decl x)
             (assign x y)
             (if (gt x 3)
                 (
                  (decl z)
                  (assign x (* x y))
                  )
                 )
             )) ;; ((x 16) (y 4)) for ((y 4)) ((x 1) (y 1)) for ((y 1))

(define q6 '((decl x)
             (assign x y)
             (if (gt x 3)
                 (
                  (assign x 20)
                 )
             )
             (if (gt x 15)
                 (
                   (assign x 11)
                 )  
             )
             )) ;; ((y 1)) -> ((x 1) (y 1)) or ((y 4)) -> ((x 11) (y 4))

(define q7 '(
             (decl y)
             (if (gt x 3)
                 (
                  (decl z)
                  (assign y (+ z 1))
                 )
             )
             (if (lt x 4)
                 (
                  (decl w)
                  (assign y (+ (+ w 1) 1))
                 )
             )))
#|
> (sem q7 '((x 1)))
'((y 2) (x 1))
> (sem q7 '((x 4)))
'((y 1) (x 4))
> 
|#


(define q8 '(
             (decl y)
             (if (gt x 3)
                 (
                  (assign y x)
                  (if (gt y (- x 1))
                      (
                       (decl z)
                       (assign z 10)
                       (assign y (+ x z))
                       (if (gt y 11)
                           (
                            (decl z)
                            (assign z 11)
                            (assign y (+ x z))
                           )
                        )
                       )
                  )
                  (assign y (+ y y)) 
                 )
              )
             ))
#|
> (sem q8 '((x 1)))
'((y 0) (x 1))
> (sem q8 '((x 4)))
'((y 30) (x 4))
> 
|#

(define q9 '(
             (decl x1)
             (decl x2)
             (decl x3)
             (assign x1 y1)
             (assign x2 y2)
             (assign x3 y3)
             (if (gt x1 x2)
                 (
                  (decl t)
                  (assign t x1)
                  (assign x1 x2)
                  (assign x2 t)
                 )
            )
            (if (gt x1 x3)
                (
                  (decl t)
                  (assign t x1)
                  (assign x1 x3)
                  (assign x3 t)
                )
            )
            (if (gt x2 x3)
                (
                  (decl t)
                  (assign t x2)
                  (assign x2 x3)
                  (assign x3 t)
                 )
            )
          ))
#|
> (sem q9 '((y1 11) (y2 10) (y3 9)))
'((x3 11) (x2 10) (x1 9) (y1 11) (y2 10) (y3 9))
|#

(define q10
      '(
       (decl x)
       (assign x 1)
       (decl i)
       (assign i 1)
       (while (lt i n)
              (
                (assign x (* x i))
                (assign i (+ i 1))
               ))
      ))
      
(define q11
      '(
       (decl f1)
       (decl f2)
       (assign f2 1)
       (decl i)
       (assign i 1)
       (while (lt i n)
              (
               (decl t)
               (assign t f2)
               (assign f2 (+ f1 f2))
               (assign f1 t)
               (assign i (+ i 1))
               ))
      ))


(define q12
  '(
    (decl a)
    (decl i)
    (while (lt i n)
           (
            (decl j)
            (assign j (+ i 1))
            (while (lt j n)
                   (
                    (assign a (+ a 1))
                    (assign j (+ j 1))
                    ))
            (assign i (+ i 1))
            ))
    ))