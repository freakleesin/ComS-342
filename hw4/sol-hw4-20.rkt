#lang racket
(require racket/trace)
(provide (all-defined-out))
(require "program.rkt")


#|
synchk: Ps -> {true, false}

- Program \in Ps is a list of statements
- Statement is a list of three forms: decl, assign, if
|#

(define (synchk P)
  (if (and (list? P) (not (null? P)))  ;; should be a non-empty list
      (if (null? (cdr P))  ;; P contains just one
          (synchkStmt (car P)) ;; synchk the statement
          (and (synchkStmt (car P))  ;; else synchk the sequence of statements
               (synchk (cdr P))))
      false))  ;; everything else is false.

;; 
(define (synchkStmt S)
  (and (and (list? S) (not (null? S)))  ;; should be a non-empty list
       (or
        ;; declara
        (and
             (equal? (car S) 'decl)
             (equal? (length S) 2)
             (symbol? (cadr S)))
     
        ;; assignment 
        (and (equal? (car S) 'assign)
             (equal? (length S) 3)
             (symbol? (cadr S))
             (arithExpr (cadr (cdr S))))
        ;; if and while
        (and (or (equal? (car S) 'if) (equal? (car S) 'while))
             (equal? (length S) 3)
             (condExpr (cadr S))
             (synchk (cadr (cdr S))))
        )))

(define (arithExpr E)
  (or
   ;; number
   (number? E)
   ;; variable   
   (symbol? E)
   ;; operator operand operand   
   (and (list? E)
        (or (equal? (car E) '+) (equal? (car E) '-) (equal? (car E) '/) (equal? (car E) '*) )
        (equal? (length E) 3)
        (arithExpr (cadr E))
        (arithExpr (cadr (cdr E))))))
  
(define (condExpr E)
  (and (list? E)
       (or
        ;; gt, lt, eq
        (boolExpr E)
        ;; or, and, 
        (and
         (or (equal? (car E) 'and) (equal? (car E) 'or))
         (equal? (length E) 3)
         (condExpr (cadr E))
         (condExpr (cadr (cdr E))))
        ;; not
        (and
         (equal? (car E) 'not)
         (equal? (length E) 2)
         (condExpr (cadr E))))))
  
(define (boolExpr E)
  (and
       (or (equal? (car E) 'gt) (equal? (car E) 'lt) (equal? (car E) 'eq))
       (arithExpr (cadr E))
       (arithExpr (cadr (cdr E)))))

#| For further practice: restructure the above and have a function for checking
                         each expression rather than using nested or's and and's
 |#

#|
 Semantics of a program =
  Semantics of rest of the program in the context of semantics of first statement
|#

(define (sem P Env)
  (if (null? (cdr P))  ; one statement in the program 
      (semstmt (car P) Env)  ;; find the semantics of the single statement
      (sem (cdr P) (semstmt (car P) Env))))  ;; else relay-chain the semantics

(define (semstmt S Env)
  (cond
    ;[ (equal? 1 1) 1 ]
    ;; declaration 
    [ (equal? (car S) 'decl)   (cons (list (cadr S) 0) Env) ]
    ;; assignment: update the value of the variable
    [ (equal? (car S) 'assign) (updateValue (cadr S)
                                            (semArith (cadr (cdr S)) Env)
                                            Env) ]
    ;; if: setup a marker for the start of the if and when all is done, remove the environment till the marker (incl)
    [ (equal? (car S) 'if)  (removemarker (semIf (semcomplexcond (cadr S) Env) ;; condExpr
                                                 (cadr (cdr S)) ;; sequence of stmts
                                                 (cons (list '$if 0) Env))) ]
    [ (equal? (car S) 'while) (if (semcomplexcond (cadr S) Env) ;; condition is true
                                  (sem (list (list 'if '(eq 1 1) (cadr (cdr S))) ;; sequence of while statements
                                               S) ;; the while statement
                                       Env)
                                  Env) ] ;; return the environment because nothing is changing
    ))

#| (while Cond (SSeq)): if semantics of Cond is true then semantics of while-stmt
   is equivalent to
   ( (if (eq 1 1) (SSeq)) (while Cond (SSeq)) )
   This is implemented in the above semantic rule for while
|#

#| semIf: parameter 1 is either true/false
          parameter 2 is sequence of statements in if-bod
          parameter 3 is the environment obtained by adding ('$if 0) at the top of existing
                      environment. Any marker can be used: probably it is better to use
                      integer as a marker.
|#
(define (semIf condVal SSeq Env)
  (if condVal
      (sem SSeq Env)
      Env))

#|
remove everything in the Environment till the first marker
Takes care of nesting. 
|#
(define (removemarker Env)
  (if (equal? (car (car Env)) '$if)
      (cdr Env)
      (removemarker (cdr Env))))

#|
update the associated value of the first occurrence of v with val in the Env
|#
(define (updateValue v val Env)
  (if (equal? (car (car Env)) v)
      (cons (list (car (car Env))
                  val)
            (cdr Env))
      (cons (car Env) (updateValue v val (cdr Env)))))

#|
Code developed in-class starts here.
find the associated value of a variable (first occurrence)
|#
(define (findValue v Env)
  (if (equal? v (car (car Env)))
      (cadr (car Env))
      (findValue v (cdr Env))))

; semantics of arith expression
(define (semArith Expr Env)
  (cond
    [ (number? Expr)          Expr ]

    [ (symbol? Expr)          (findValue Expr Env) ]
    
    [ (equal? (car Expr) '+)  (+ (semArith (cadr Expr)  Env)
                                 (semArith (cadr (cdr Expr)) Env)) ]
    [ (equal? (car Expr) '-)  (- (semArith (cadr Expr) Env)
                                 (semArith (cadr (cdr Expr)) Env)) ]
    [ (equal? (car Expr) '*)  (* (semArith (cadr Expr) Env)
                                 (semArith (cadr (cdr Expr)) Env)) ]
    [ (equal? (car Expr) '/)  (/ (semArith (cadr Expr) Env)
                                 (semArith (cadr (cdr Expr)) Env)) ]
    ))


; semantics of complex conditions
(define (semcomplexcond CCond Env)
  (cond
    [ (equal? (car CCond) 'or)   (or (semcomplexcond (cadr CCond) Env)
                                     (semcomplexcond (cadr (cdr CCond)) Env)) ]
    [ (equal? (car CCond) 'and)   (and (semcomplexcond (cadr CCond) Env)
                                     (semcomplexcond (cadr (cdr CCond)) Env)) ]
    [ (equal? (car CCond) 'not)   (not (semcomplexcond (cadr CCond) Env))
                                      ]
    [ else  (semboolcond CCond Env) ]))  ;; semantics of conditions: lt, gt

; complete the definition
(define (semboolcond BCond Env)
  (cond
    [ (equal? (car BCond) 'gt)  (> (semArith (cadr BCond) Env)
                                   (semArith (cadr (cdr BCond)) Env)) ]
    [ (equal? (car BCond) 'lt)  (< (semArith (cadr BCond) Env)
                                   (semArith (cadr (cdr BCond)) Env)) ]
    [ (equal? (car BCond) 'eq)  (equal? (semArith (cadr BCond) Env)
                                        (semArith (cadr (cdr BCond)) Env)) ]))



