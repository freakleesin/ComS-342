;#lang racket ;; uncomment this line
;(require "sol-hw4-20.rkt") ;; include your solution here.
;(require racket/trace)
(require "program.rkt")
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
                                   ))])
      (if (eval testfun ns)
          (begin
            (writeln "correct")
            (set! totalpoints (+ totalpoints testpoints)))
          (begin
            (writeln "incorrect output")
            ))
    )
    ))

(define (hw4)
  (begin

    (writeln '************************************************)
    (writeln 'Tests-on-Q1)
    (writeln '************************************************)

    
    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt ':synchk-p0 '(not (synchk p0)) 1)
                               

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-p1 '(not (synchk p1)) 2)
    
    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-p2 '(not (synchk p2)) 2)
  
    (writeln '--------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt ':synchk-p3 '(not (synchk p3)) 2)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-p4 '(not (synchk p4)) 2)

    (writeln "--------------")
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-p5 '(not (synchk p5)) 1.5)
    
    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-p6 '(not (synchk p6)) 2)
    
    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-p7 '(not (synchk p7)) 2)
        
    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-p8 '(not (synchk p8)) 2)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-p9 '(not (synchk p9)) 2)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-p10 '(not (synchk p10)) 2.5)


    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-p11 '(not (synchk p11)) 3)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-p12 '(not (synchk p12)) 3)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-p13 '(synchk p13) 1)
    
    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-q0 '(synchk q0) 1)


    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-q1 '(synchk q1) 1)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-q2 '(synchk q2) 1)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-q3 '(synchk q3) 1)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-q4 '(synchk q4) 1)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-q5 '(synchk q5) 1)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-q6 '(synchk q6) 1)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-q7 '(synchk q7) 1)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-q8 '(synchk q8) 1)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-q9 '(synchk q9) 1)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-q10 '(synchk q10) 1)

    (writeln '--------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':synchk-q10 '(synchk q11) 1)
    
    (writeln '************************************************)
    (writeln 'Tests-on-Q2)
    (writeln '************************************************)
    
    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q0 '(myequal (sem q0 '((y 1))) '((x 1) (y 1))) 3)
        
    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q1 '(myequal (sem q1 '()) '((x 1))) 3)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q2 '(myequal (sem q2 '()) '((x 1) (y 2))) 4)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q3a '(myequal (sem q3 '((y 0))) '((x 0) (y 0))) 3)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q3b '(myequal (sem q3 '((y 10))) '((x 10) (y 10))) 3)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q4a '(myequal (sem q4 '((y 1))) '((x 1) (y 1))) 4)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q4b '(myequal (sem q4 '((y 4))) '((x 4) (y 4))) 4)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q5a '(myequal (sem q5 '((y 4))) '((x 16) (y 4))) 3)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q5b '(myequal (sem q5 '((y 1))) '((x 1) (y 1))) 3)


    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q6a '(myequal (sem q6 '((y 1))) '((x 1) (y 1))) 3)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q6b '(myequal (sem q6 '((y 4))) '((x 11) (y 4))) 3)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q7a '(myequal (sem q7 '((x 1))) '((x 1) (y 2))) 3)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q7b '(myequal (sem q7 '((x 4))) '((x 4) (y 1))) 3)


    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q8a '(myequal (sem q8 '((x 1))) '((x 1) (y 0))) 3)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q8b '(myequal (sem q8 '((x 4))) '((x 4) (y 30))) 3)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q9 '(myequal (sem q9 '((y1 11) (y2 10) (y3 9)))
                                  '((x3 11) (x2 10) (x1 9) (y1 11) (y2 10) (y3 9))) 3)


    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q10 '(myequal (sem q10 '((n 5))) '((i 5) (x 24) (n 5))) 3)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q11 '(myequal (sem q11 '((n 5))) '((i 5) (f2 5) (f1 3) (n 5))) 3)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q12 '(myequal (sem q12 '((n 10))) '((i 10) (a 45) (n 10))) 3)
    
    (writeln '---------------------------------------)
    (write "                      Total Points: ")
    (writeln totalpoints)

    
    )
)

(hw4)

