;;#lang racket ;; uncomment this line
;; (require "sol-opt2-20.rkt") ;; include your solution here

;(require racket/trace)
(require "tests.rkt")
(provide (all-defined-out))
(require racket/sandbox)


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
    (with-handlers ([exn:fail? (lambda (exn) (begin (writeln "4sec Timeout"))) ])
    (with-deep-time-limit 4 
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
    )
    )  
    ))

(define (hw5)
  (begin

    (writeln '************************************************)
    (writeln 'Tests-on-Q1)
    (writeln '************************************************)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':vsem-f1 '(myequal (vsem f1 mutexst mutextr) '()) 8)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':vsem-f2 '(myequal (vsem f2 mutexst mutextr) '(s0 s1 s2 s3 s4 s5)) 8)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':vsem-f3a '(myequal (vsem f3 mutexst mutextr) '(s0 s1 s2 s3 s4 s5)) 9)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':vsem-f3b '(myequal (vsem f3 mutexst mutextr1) '()) 9)
    
    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':vsem-f4 '(myequal (vsem f4 mutexst mutextr) '(s1 s3 s4)) 8)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':vsem-f5 '(myequal (vsem f5 mutexst mutextr) '(s0 s1 s3 s4)) 9)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':vsem-f6 '(myequal (vsem f6 mutexst mutextr) '(s0 s1 s2 s4 s5)) 9)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':vsem-m3f1 '(myequal (vsem m3f1 m3states m3trans) '(s1 s2)) 8)

    
    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':vsem-m3f2 '(myequal (vsem m3f2 m3states m3trans) '(s0 s1 s2)) 8)
    
    
    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':vsem-m4f1 '(myequal (vsem m4f1 m4states m4trans) '(s2 s4 s5)) 8)
    
    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':vsem-m4f2 '(myequal (vsem m4f2 m4states m4trans)'(s0 s1 s2 s4 s5)) 8)
    
    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':vsem-m5f1 '(myequal (vsem m5f1 m5states m5trans) '(s0 s1)) 8)
    
    (writeln '---------------------------------------)
    (write "                      Total Points: ")
    (writeln totalpoints)

    
    )
)

(hw5)

