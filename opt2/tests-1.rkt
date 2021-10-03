#lang racket
(provide (all-defined-out))

#| Test Models 1 and 2 with properties |#

(define mutexst
  '( (s0 (w1 w2))
     (s1 (t1 w2))
     (s2 (c1 w2))
     (s3 (t1 t2))
     (s4 (w1 t2))
     (s5 (w1 c2))))

(define mutextr
  '( (s0 s1)
     (s1 s2)
     (s2 s0)
     (s1 s3)
     (s3 s1)
     (s3 s4)
     (s0 s4)
     (s4 s3)
     (s4 s5)
     (s5 s0)))

(define mutextr1
  '( (s0 s1)
     (s1 s2)
     (s2 s0)
     (s1 s3)
     (s3 s3)
     (s0 s4)
     (s4 s3)
     (s4 s5)
     (s5 s0)))




(define f1 '(not (starex (not (or (not (prop t1))
                              (starax (prop c1)))))))
; ()

(define f2 '(starex (and (prop t1)
                     (not (starax (prop c1))))))
;; s0 s1 s2 s3 s4 s5

(define f3 '(not (starex (not (or (not (prop t1))
                              (starex (prop c1)))))))

;; ()

(define f4 '(not (starax (not (or (prop t1) (prop t2))))))
; s1 s3 s4

(define f5 '(ex (not (starax (not (prop t1))))))
                         ; '(s0 s1 s3 s4)

(define f6 '(not (starax (not (or (prop w1) (prop w2))))))
; '(s0 s1 s2 s4 s5)



#| Test Model 3 w/ properties |#

(define m3states
  '((s0 (p q))
    (s1 ())
    (s2 (p))))

(define m3trans
  '((s0 s0)
    (s0 s1)
    (s1 s2)
    (s2 s2)))

(define m3f1 '(starax (not (starex (not (prop p))))))
(define m3f2 '(not (starex (not (starax (prop p))))))

#|
> (vsem m3f1 m3states m3trans)
'(s1 s2)
> (vsem m3f2 m3states m3trans)
'(s0 s1 s2)
|#

#| Test Model 4 w/ properties |#
(define m4states
  '((s0 (p))
    (s1 (q1))
    (s2 (q2))
    (s3 (r))
    (s6 (q2))
    (s4 ())
    (s5 (q1 r))))

(define m4trans
  '((s0 s1)
    (s1 s3)
    (s1 s4)
    (s2 s4)
    (s4 s5)
    (s3 s6)
    (s6 s6)
    (s5 s5)))

(define m4f1 '(starax (ax (prop r))))
(define m4f2 '(ax (starax (prop r))))
#|
> (vsem m4f1 m4states m4trans)
'(s2 s4 s5)
> (vsem m4f2 m4states m4trans)
'(s0 s1 s2 s4 s5)
>
|#

#| Test Model 5 w/ properties |#

(define m5states
  '((s0 (p))
    (s1 (q))
    (s3 ())
    (s2 (r))))

(define m5trans
  '((s0 s1)
    (s0 s2)
    (s3 s3)
    (s1 s0)
    (s2 s3)))

(define m5f1 '(not ( starax (not (starex (prop r))))))
#|
> (vsem m5f1 m5states m5trans)
'(s0 s1)
|#