#lang racket
(require "tests.rkt")
(provide (all-defined-out))


;Q1
(define (swap s)
  (if (< (length s) 3)
      s
      (cons (caddr s) (cons (cadr s) (cons (car s) (cdddr s))))))

(define (checkST c1 c2)
  (if (<= (car c1) (car c2))
      #t
      #f))

(define (checkFT c1 c2)
  (if (<= (cadr c1) (cadr c2))
      #t
      #f))

(define (checkSH c1 c2)
  (if (< (- (cadr c1) (car c1)) (- (cadr c2) (car c2)))
      #t
      #f))

(define (checkT r1 r2)
  (if (<= r1 r2)
      #t
      #f))

(define (ifEq l1 l2)
  (if (equal? l1 l2)
      #t
      #f))

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (div l n)
  (if (< n 1)
      (list '() l)
      (list (cons (car l) (car (div (cdr l) (- n 1)))) (cadr (div (cdr l) (- n 1))))))

(define (merge2 l1 l2 jud)
  (if (null? l1)
      l2
      (if (null? l2)
          l1
          (if (jud (car l1)(car l2))
              (cons (car l1) (merge2 (cdr l1) l2 jud))
              (cons (car l2) (merge2 l1 (cdr l2) jud))))))

(define (merge l jud)
  (if (eq? (length l) 1)
      l
      (merge2 (merge (car (div l (/ (length l) 2))) jud) (merge (cadr (div l (/ (length l) 2))) jud) jud)))

(define (checkOrder c1 c2)
  (not (ifEq (append (car c1) (cadr c1)) c2)))

(define (checkOl c1 c2)
  (checkOrder (merge (list c1 c2) checkST) (merge (append c1 c2) checkT)))

(define (checkAllOl r t)
  (if (eq? (length r) 1)
      (checkOl (car r) t)
      (or (checkAllOl (cdr r) t) (checkOl (car r) t))))

(define (arrange l lb)
  (if (null? l)
      lb
      (if (null? lb)
          (arrange (cdr l) (list (car l)))
          (if (checkAllOl lb (car l))
              (arrange (cdr l) lb)
              (arrange (cdr l) (append lb (list (car l))))))))

(define (arrangeST l)
  (arrange (merge l checkST) '()))

(define (arrangeFT l)
  (arrange (merge l checkFT) '()))

(define (arrangeSH l)
  (cons (car (arrange (merge l checkSH) '())) (swap (cdr (arrange (merge l checkSH) '())))))

(define (getreqlst l dif)
  (cond
    [(eq? 'st dif) (arrangeST l)]
    [(eq? 'ft dif) (arrangeFT l)]
    [(eq? 'sh dif) (arrangeSH l)]))


(define requests'((0 8) (8 11) (1 3) (4 13) (5 6) (3 10) (7 9) (10 12)))






;Q2
(define (replace expr k v)
  (cond
    ((symbol? expr) (if (eq? expr k) v expr))
    ((eq? (car expr) 'lambda)
     (if (eq? (cadr expr) k)
         expr
         (list 'lambda (cadr expr) (lsem (replace (caddr expr) k v)))))
    (else (list (lsem (replace (car expr) k v))
                (lsem (replace (cadr expr) k v))))))


(define (lsem expr)
  (cond
    ((symbol? expr) expr)
    ((eq? (car expr) 'lambda) expr)
    (else
     (cond
       ((symbol? (car expr)) expr)
       
       ((eq? (car (car expr)) 'lambda)
        (lsem (replace  (caddr (car expr))
                        (cadr (car expr))
                        (cadr expr))))
       (else (lsem (list (lsem (car expr)) (cadr expr))))))))




(getreqlst requests 'st)
(getreqlst requests 'ft)
(getreqlst requests 'sh)

(lsem '((lambda x x) y))
(lsem '(((lambda f (lambda x (f x))) g) z))
(lsem '( (lambda n (lambda f (lambda x (f ((n f) x)))))(lambda g (lambda y y))))