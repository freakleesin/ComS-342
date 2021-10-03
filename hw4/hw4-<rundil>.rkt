#lang racket
(require "program.rkt")
(provide (all-defined-out))


(define (checkDecl d)
  (and
   (= 2 (length d))
   (eq? (car d) 'decl)
   (symbol? (cadr d))))

(define (checkArithExpr e)
  (or (number? e)(symbol? e)
      (checkOpExpr e)))

(define (checkAssign a)
  (and
   (= 3 (length a))
   (eq? (car a) 'assign)
   (symbol? (cadr a))
   (checkArithExpr (caddr a))))

(define (checkOp op)
  (or (eq? '+ op)
      (eq? '- op)
      (eq? '* op)
      (eq? '/ op)))

(define (checkOpExpr e)
  (and (= 3 (length e))
       (checkOp (car e))
       (checkArithExpr (cadr e))
       (checkArithExpr (caddr e))))
       
(define (checkCondOp op)
  (or (eq? op 'gt)
      (eq? op 'lt)
      (eq? op 'eq)))

(define (checkBiCondOp op)
  (or (eq? op 'or)
      (eq? op 'and)))

(define (checkBinCond c)
  (and (= 3 (length c))
       (checkBiCondOp (car c)) (checkCondExpr (cadr c)) (checkCondExpr (caddr c))))

(define (checkUCond c)
  (and (= 2 (length c))
       (eq? 'not (car c)) (checkCondExpr (cadr c))))

(define (checkCondExpr e)
  (or (checkBCond e)
      (checkBinCond e)
      (checkUCond e)))

(define (checkBCond c)
  (and (= 3 (length c))
       (checkCondOp (car c))
       (checkArithExpr (cadr c))
       (checkArithExpr (caddr c))))


(define (checkIf i)
  (and (= 3 (length i))
       (eq? 'if (car i))
       (checkCondExpr (cadr i))
       (checkSSeq (caddr i))))

(define (checkWhile w)
  (and (= 3 (length w))
       (eq? 'while (car w))
       (checkCondExpr (cadr w))
       (checkSSeq (caddr w))))

(define (checkStatement s)
  (or (checkDecl s) (checkAssign s) (checkIf s) (checkWhile s)))

(define (checkStats s)
  (if (eq? s '())
      #t
      (and (checkStatement (car s)) (checkStats (cdr s)))))
      
(define (checkSSeq list)
  (cond
    ((not (list? list)) #f)
    ((eq? list '()) #f)
    (else  (checkStats list))))

(define (synchk p)
  (checkSSeq p))



(define (lookup name l)
  (if
   (eq? l '()) '()
   (if
    (eq? name  (car (car l)))
    (cadr (car l))
    (lookup name (cdr l)))))

(define (lookupAll name ls)
  (if
   (eq? ls '()) '()
   (if (eq? '() (lookup name (car ls)))
       (lookupAll name (cdr ls))
       (lookup name (car ls)))))

(define (update env n v)
  (if
   (eq? env '()) '()
   (if (eq? n (car (car env)))
       (cons (list n v) (cdr env))
       (cons (car env) (update (cdr env) n v)))))

(define (updateAll envs n v)
  (if
   (eq? envs '()) '()
   (if
    (eq? '() (lookup n (car envs)))
    (cons (car envs) (updateAll (cdr envs) n v))
    (cons (update (car envs) n v)
           (cdr envs)))))

(define (addPair ls n v)
  (cons (cons (list n v) (car ls) ) (cdr ls)))

(define (evalDecl envs s)
  (addPair envs (cadr s) 0))

(define (evalArithExpr envs s)
  (cond
    ((number? s) s)
    ((symbol? s) (lookupAll  s envs) )
    (else (evalOpArith envs s))))


(define (evalOpArith envs s)
  (cond
    ((eq? '+ (car s)) (+ (evalArithExpr envs (cadr s)) (evalArithExpr envs (caddr s))))
    ((eq? '- (car s)) (- (evalArithExpr envs (cadr s)) (evalArithExpr envs (caddr s))))
    ((eq? '* (car s)) (* (evalArithExpr envs (cadr s)) (evalArithExpr envs (caddr s))))
    ((eq? '/ (car s)) (/ (evalArithExpr envs (cadr s)) (evalArithExpr envs (caddr s))))))


(define (evalAssign envs s)
  (updateAll envs (cadr s) (evalArithExpr envs (caddr s))))

(define (evalCondExpr envs s)
  (cond

    ((eq? 'gt (car s)) (> (evalArithExpr envs (cadr s)) (evalArithExpr envs (caddr s))  ))
    ((eq? 'lt (car s)) (< (evalArithExpr envs (cadr s)) (evalArithExpr envs (caddr s))  ))
    ((eq? 'eq (car s)) (= (evalArithExpr envs (cadr s)) (evalArithExpr envs (caddr s))  ))
    ((eq? 'or (car s)) (or (evalCondExpr envs (cadr s)) (evalCondExpr envs (caddr s))  ))
    ((eq? 'and (car s)) (and (evalCondExpr envs (cadr s)) (evalCondExpr envs (caddr s))  ))
    ((eq? 'not (car s)) (not (evalCondExpr envs (cadr s))))))
    
(define (evalIf envs s)
  (if (evalCondExpr envs (cadr s))
      (cdr (evalSSeq (cons '() envs) (caddr s)))
      envs))

(define (evalWhile envs s)
  (if (evalCondExpr envs (cadr s))
      (evalWhile  (cdr (evalSSeq (cons '() envs) (caddr s))) s)
      envs))

(define (evalStatement envs s)
  (cond
    ((eq? 'decl (car s)) (evalDecl envs s))
    ((eq? 'assign (car s)) (evalAssign envs s))
    ((eq? 'if (car s)) (evalIf envs s))
    ((eq? 'while (car s)) (evalWhile envs s))))

(define (evalSSeq envs l)
  (if (eq? l '())
      envs
      (evalSSeq (evalStatement envs (car l)) (cdr l))))

(define (sem expr env)
  (car (evalSSeq (list env) expr)))

(define program1
  '(
    (decl x)
    (assign x 3)
    (decl y)
    (assign y 10)
    (if (gt x 2)
        (
         (decl x)
         (assign x y)
         (assign x (+ x y))
         ) )

    (assign x (+ x 1))
    ) )

(define program2

  '(
    (decl x)
    (decl z)
    (assign x (+ y 1))
    (if (gt x 1)
        ((assign z 1))
        )
    (if (gt x 2)
        ((assign z 2))
        )))

(synchk program1)
(synchk program2)

(sem program1 '())
(sem program1 '((x 20)))
(sem program2 '((y 10)))
(sem program2 '((y 0)))