#lang racket
(require "tests.rkt")
(provide (all-defined-out))

(define (lookup attr value)
  (if (null? attr)
      null
      (if (equal? (car (car attr)) value)
          (cadr (car attr))
          (lookup (cdr attr) value))))

(define (checkPref fst sec attr)
  (if (equal? fst sec)
      #t
      (if (null? (lookup attr fst))
          #f
          (helper attr (lookup attr fst) sec))))

(define (helper attr lst sec)
  (if (null? lst)
      #f
      (if (or (checkPref (car lst) sec attr) (equal? (car lst) sec))
          #t
          (helper attr (cdr lst) sec))))

(define (compare attr1 attr2 attr3 fst sec)
  (cond
   [(equal? fst sec) null]
   [(and (checkPref (car sec) (car fst) attr1) (checkPref (cadr sec) (cadr fst) attr2) (checkPref (cadr (cdr sec)) (cadr (cdr fst)) attr3)) 't]))

(define (addnew lst attr1 attr2 attr3 fst sec lst2)
  (if (null? lst2)
      (list fst sec)
      (if (equal? (checkSeq lst attr1 attr2 attr3 (car lst2)) #f)
          (addnew lst attr1 attr2 attr3 fst (append sec (list (car lst2))) (cdr lst2))
          (addnew lst attr1 attr2 attr3 (append fst (list (car lst2))) sec (cdr lst2)))))

(define (checkSeq lst attr1 attr2 attr3 check)
  (if (null? lst)
      #t
      (if (equal? (compare attr1 attr2 attr3 check (car lst)) 't)
          #f
          (checkSeq (cdr lst) attr1 attr2 attr3 check))))

(define (weakorderHelper1 carlist attr1 attr2 attr3 tier)
  (append tier (list (car (addnew carlist attr1 attr2 attr3 '() '() carlist)))))

(define (weakorderHelp2 carlist attr1 attr2 attr3 tier)
  (if (null? carlist)
      tier
      (weakorderHelp2 (cadr (addnew carlist attr1 attr2 attr3 '() '() carlist)) attr1 attr2 attr3 (weakorderHelper1 carlist attr1 attr2 attr3 tier))))

(define (weakorder carlist attr1 attr2 attr3)
  (weakorderHelp2 carlist attr1 attr2 attr3 '()))