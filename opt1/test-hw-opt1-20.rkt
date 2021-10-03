;#lang racket ;; uncomment this line
;(require "sol-opt1-20.rkt") ;; include your file
(require "tests.rkt")
(provide (all-defined-out)) 


(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (myequal l1 l2)
  (if (not (equal? (length l1) (length l2)))
      false
      (myeqlow l1 l2)))

(define (myeqlow l1 l2)
  (if (null? l1)
      true
      (and (present (car l1) l2)
          (myeqlow (cdr l1) l2))))

;; does some specific course present in the course list
(define (present x lst)
  (if (null? lst)
      false
      (if (equal? x (car lst))
          true
          (present x (cdr lst)))))



(define totalpoints 0) ;; total points for this assignment
(define cnt 0)  ;; test counts


(define (utest testcnt testname testfun testpoints)
  (begin
    (write testcnt)
    (write testname)
    (write ':)
    (write testpoints)
    (writeln 'pts)
    (with-handlers ([exn:fail? (lambda (exn)
                                 (begin
                                   (writeln exn)
                                   (writeln "Exception")
                                   (writeln "incorrect")
                                   ;(set! totalpoints (- totalpoints testpoints))
                                   ))])
      (if (eval testfun ns)
          (begin
            (writeln "correct")
            (set! totalpoints (+ totalpoints testpoints)))
          (begin
            (writeln "incorrect output")
            ;(set! totalpoints (- totalpoints testpoints))
            ))
    )
    ))


(define (opt1)
  (begin

    (writeln '************************************************)
    (writeln 'Tests-on-Q1)
    (writeln '************************************************)

(writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':r0-st '(myequal (getreqlst r0 'st) '((0 10) (10 13))) 5)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':r0-ft '(myequal (getreqlst r0 'ft) '((1 5) (5 7) (7 8) (10 13))) 5)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':r0-sh '(myequal (getreqlst r0 'sh) '((7 8) (5 7) (10 13) (1 5))) 5)


    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':r1-st '(myequal (getreqlst r1 'st) '((0 6) (6 10))) 5)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':r1-ft '(myequal (getreqlst r1 'ft) '((1 3) (3 8) (8 21))) 5)

    (writeln '--------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt ':r1-sh '(or (myequal (getreqlst r1 'sh) '((1 3) (5 9)))
                            (myequal (getreqlst r1 'sh) '((1 3) (6 10)))) 5)


    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':r2-st '(myequal (getreqlst r2 'st) '((0 19))) 5)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':r2-ft '(myequal (getreqlst r2 'ft) '((1 6) (7 13))) 5)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':r2-sh '(myequal (getreqlst r2 'sh) '((4 8))) 5)
  


    (writeln '************************************************)
    (writeln 'Tests-on-Q2)
    (writeln '************************************************)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':lsem-e1 '(equal? (lsem e1) '(lambda x z)) 4)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':lsem-e2 '(equal? (lsem e2) '(lambda x y)) 4)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':lsem-e3 '(equal? (lsem e3) '(lambda x x)) 4)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':lsem-e4 '(equal? (lsem e4) '(z1 z2)) 6)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':lsem-e5 '(equal? (lsem e5) '(p p)) 6)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':lsem-e6 '(equal? (lsem e6) '(lambda f (lambda x (f x)))) 7)
    
    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':lsem-e7 '(equal? (lsem e7) '(lambda f1 (lambda x1 (f1 (f1 (f1 x1)))))) 12)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':lsem-e8 '(equal? (lsem e8) '(lambda f1 (lambda x1 (f1 (f1 (f1 (f1 (f1 (f1 x1))))))))) 12)
    )
  
  (writeln '---------------------------------------)
  (write "                      Total Points: ")
  (writeln totalpoints)
)

(opt1)

