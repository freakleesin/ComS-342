;#lang racket ;; uncomment this line
; include your solution
(require "tests.rkt")
(provide (all-defined-out))
(require racket/sandbox)


(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (myeqweak l1 l2)
  (if (not (equal? (length l1) (length l2)))
      false
      (myeqweaklow l1 l2)))

(define (myeqweaklow l1 l2)
  (or (null? l1)
      (and (myequal (car l1) (car l2))
           (myeqweaklow (cdr l1) (cdr l2)))))

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
                                
                                   ))])
      (if (eval testfun ns)
          (begin
            (writeln "correct")
            (set! totalpoints (+ totalpoints testpoints)))
          (begin
            (writeln "incorrect output")
            
            ))
    )
    )
    )  
    ))

(define (opt3)
  (begin

    (writeln '************************************************)
    (writeln 'Tests-on-Q1)
    (writeln '************************************************)

    (writeln '-------------)
    (set! cnt (+ cnt 1))


    
    (utest cnt ': '(myeqweak (weakorder plist a1 a2 a3)
                              '(((white android2 htc)
                                 (blue android1 samsung)
                                 (black android1 zte)
                                 (red android1 htc)
                                 (blue android1 samsung)
                                 (red android2 zte))
                                ((blue android2 pixel)
                                 (blue android2 samsung)))) 8)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ': '(myeqweak (weakorder clist1 b1 b2 b3)
                              '(((red electric tesla) (black hybrid tesla))
                                ((red electric bmw))
                                ((red four skoda)))) 8)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ': '(myeqweak (weakorder clist1 e1 e2 e3)
                              '(((red electric tesla) (black hybrid tesla)
                                 (red electric bmw) (red four skoda)))) 6)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ': '(myeqweak (weakorder clist1 b1 b21 b3)
                             '(((red electric tesla)
                                (black hybrid tesla)
                                (red four skoda))
                               ((red electric bmw)))) 6)


    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ': '(myeqweak (weakorder clist2 b1 b2 b3)
                             '(((black hybrid bmw)
                                (blue electric tesla) (black electric skoda))
                               ((red electric tesla)
                                (blue electric bmw)
                                (red hybrid bmw) (black four alfa))
                               ((red four alfa)))) 8)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ': '(myeqweak (weakorder clist3 b1 b2 b3)
                             '(((black electric tesla) (black hybrid tesla))
                               ((black four tesla) (white electric tesla) (white hybrid tesla))
                               ((black six tesla) (blue electric tesla) (blue hybrid tesla) (white four tesla))
                               ((red electric tesla) (red hybrid tesla) (blue four tesla) (white six tesla))
                               ((red four tesla) (blue six tesla))
                               ((red six tesla)))) 8)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ': '(myeqweak (weakorder clist4 b1 b2 b3)
                             '(((black electric tesla) (black hybrid bmw))
                               ((black electric bmw) (black hybrid skoda))
                               ((black four alfa) (black electric skoda))
                               ((black six alfa)))) 8)


    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ': '(myeqweak (weakorder clist5 b1 b2 b3)
                             '(((blue electric tesla)
                                (blue hybrid tesla)
                                (black electric skoda)
                                (white four alfa)))) 8)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ': '(myeqweak (weakorder clist1 b11 b2 b3)
                             '(((red electric tesla)
                                (black hybrid tesla))
                               ((red electric bmw))
                               ((red four skoda)))) 8)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ': '(myeqweak (weakorder clist2 b11 b2 b3)
                             '(((red electric tesla)
                                (black hybrid bmw)
                                (red hybrid bmw)
                                (blue electric tesla))
                               ((blue electric bmw)
                                (red four alfa))
                               ((black four alfa)
                                (black electric skoda)))) 8)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ': '(myeqweak (weakorder clist3 b11 b2 b3)
                             '(((blue electric tesla)
                                (blue hybrid tesla)
                                (white electric tesla)
                                (white hybrid tesla))
                               ((black electric tesla)
                                (black hybrid tesla)
                                (red electric tesla)
                                (red hybrid tesla)
                                (blue four tesla)
                                (white four tesla))
                               ((black four tesla)
                                (red four tesla)
                                (blue six tesla)
                                (white six tesla))
                               ((black six tesla)
                                (red six tesla)))) 8)



    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ': '(myeqweak (weakorder clist4 b11 b2 b3)
                             '(((black electric tesla)
                                (black hybrid bmw))
                               ((black electric bmw)
                                (black hybrid skoda))
                               ((black four alfa)
                                (black electric skoda))
                               ((black six alfa)))) 8)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ': '(myeqweak (weakorder clist5 b11 b2 b3)
                             '(((blue electric tesla)
                                (blue hybrid tesla)
                                (white four alfa))
                               ((black electric skoda)))) 8)
#| 
    (writeln '-------------)
    (set! cnt (+ cnt 1))
   
    (utest cnt ': '(myeqweak (weakorder clist4 c1 c2 c3)
                             '(((black electric tesla) (black hybrid bmw))
                               ((black electric bmw) (black hybrid skoda))
                               ((black four alfa) (black electric skoda))
                               ((black six alfa)))) 8)

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt ': '(myeqweak (weakorder clist4 d1 d2 d3)
                             '(((black electric tesla) (black hybrid bmw))
                               ((black electric bmw) (black hybrid skoda))
                               ((black four alfa) (black electric skoda))
                               ((black six alfa)))) 8)
|#                             
    (writeln '---------------------------------------)
    (write "                      Total Points: ")
    (writeln totalpoints)

    
    )
)

(opt3)

