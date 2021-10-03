#lang racket
(provide (all-defined-out))

(define a1
  '(
    (black (blue))
    (white (red))
    ))

(define a2
  '(
    (android1 (android2))
   ))

(define a3
  '(
    (htc (samsung))
    (zte (pixel))
    ))


(define plist1
  '(
    (black android1 htc)
    (blue android1 htc)
    (black android2 htc)
    (blue android2 htc)
    (black android2 samsung)
    (white android1 htc)
    (red android1 htc)
    (white android2 htc)
    (red android2 htc)
    (red android2 samsung)
   )
)  

(define plist
  '(
    (blue android2 pixel)
    (white android2 htc)
    (blue android1 samsung)
    (black android1 zte)
    (blue android2 samsung)
    (red android1 htc)
    (blue android1 samsung)
    (red android2 zte)
   ))


#|
   transitivity: for extra credits?
|#
(define b1
  '(
     (white (blue red)) 
     (black (white blue red))
     (blue (red))
     ))

(define b2
  '(
    (four (six))
    (electric (four six))
    (hybrid (four six))
    ))

(define b3
  '(
    (bmw (alfa skoda))
    (tesla (bmw alfa skoda))
    (alfa (skoda))))

(define e1 '())
(define e2 '())
(define e3 '())

(define b21 '((four (six))))

(define b11
  '(
    (white (red))
    (blue (black))          
  ))            

(define clist1
  '( (red electric tesla)
     (black hybrid tesla)
     (red electric bmw)
     (red four skoda)))

(define clist2
  '( (red electric tesla)
     (black hybrid bmw)
     (blue electric bmw)
     (red hybrid bmw)
     (red four alfa)
     (blue electric tesla)
     (black four alfa)
     (black electric skoda)))

(define clist3
  '( (black electric tesla)
     (black hybrid tesla)
     (black four tesla)
     (black six tesla)
     (red electric tesla)
     (red hybrid tesla)
     (red four tesla)
     (red six tesla)
     (blue electric tesla)
     (blue hybrid tesla)
     (blue four tesla)
     (blue six tesla)
     (white electric tesla)
     (white hybrid tesla)
     (white four tesla)
     (white six tesla)
     ))
     
(define clist4
  '( (black electric tesla)
     (black hybrid bmw)
     (black electric bmw)
     (black hybrid skoda)
     (black four alfa)
     (black six alfa)
     (black electric skoda)))

(define clist5
  '( (blue electric tesla)
     (blue hybrid tesla)
     (black electric skoda)
     (white four alfa)))





