; Problem 4
; Edison Hua and John Jakobsen

; CSc 335
; first scheme interpreter


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; tls-scheme, from chapter 10 of tls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; need to allow redefinition of initial bindings in r5rs as delivered
; by drracket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; auxiliary functions

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define first car)

(define second cadr)

(define third caddr)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))



; environments implemented as tables


(define lookup-in-table
  (lambda (name table table-f)
    (cond 
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))

(define extend-table cons)



(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (names entry)
                          (vals entry)
                          entry-f)))



(define lookup-in-entry-help
  (lambda (name names vals entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car vals))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr vals)
                                  entry-f)))))




(define new-entry build)

(define names
  (lambda (entry) (car entry)))

(define vals
  (lambda (entry) (cadr entry)))




; the top level of the interpreter

(define value
  (lambda (e)
    (meaning e (quote () ))))


(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


; supporting functions for the intepeter

; syntax-directed dispatch on expression

(define expression-to-action
  (lambda (e)
    (cond 
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote mul)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))


(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond 
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote cond))
          *cond)
         (else *application)))
      (else *application))))


; operational semantics -- the definitions of the action functions

(define *const
  (lambda (e table)
    (cond 
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))


(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)




(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))


; note that as (car (quote ())) throws an error, this definition
; amounts to saying that looking anything up in the initial table
; is impossible.
(define initial-table
  (lambda (name)
    (car (quote ()))))


(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)


; cond is a special form that takes any number of 
; cond-lines ...  if it sees an else-line, it treats
; that cond-line as if its question part were true.

(define evcon
  (lambda (lines table)
    (cond 
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))


(define else?
  (lambda (x)
    (cond 
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)



(define *cond 
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)



(define evlis
  (lambda (args table)
    (cond 
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))



(define *application
  (lambda (e table)
    (myapply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)




(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))



(define myapply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (myapply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (myapply-closure
        (second fun) vals)))))


(define myapply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       ((lambda (x) (+ x 1)) (first vals)))
      ((eq? name (quote mul))
       (* (first vals) (second vals)))
      ((eq? name (quote sub1))
       (- (first vals) 1))
      ((eq? name (quote number?))
       (number? (first vals))))))


(define :atom?
  (lambda (x)
    (cond 
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive))
       #t)
      (else #f))))

(define myapply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))


































; DESIGN IDEA: The goal is to write a pre-processor for let that transforms a let-expression into a lambda-expression.
; We noticed that a let-expression is similar to a lambda-expression.

; (let ((x 1) (y 2)) (eq? x y))
; ((lambda (x y) (eq? x y)) 1 2)

; let-formals = ((x 1) (y 2))
; There are 3 changes:
; 1. The lambda parameters are the names of the let-formals.
; 2. The applied arguments of the lambda are the values of the let-formals.
; 3. let becomes lambda.
; The body of the expressions remain unchanged.

; Therefore we conceive of 2 helper functions:
; One to collect the names of a map.
; Another to collect the values of the map.

; For every pair of (name value) inside a list, we can apply car to get the name,
; and cadr to get the value.

; We use map and apply the function "first" and another map to apply the function "second".
; This is a weak induction on the length of the list.

; let-formals-to-names - Takes a list of (name values) and returns a list of names.
(define (let-formals-to-names formals)
  (map (lambda (x) (first x)) formals))

; let-formals-to-values - Takes a list of (name values) and returns a list of values.
(define (let-formals-to-values formals)
  (map (lambda (x) (second x)) formals))


; *application is used as a template, because we are creating an applied-lambda in the form ((lambda) args*).
; In the *application function, the lambda is passed to meaning,
; and the args* is passed to evlis.

; 
; We build a lambda using a list function.
;   1. 'lambda - the word lambda denotes this is a lambda expression.
;   2. The result of let-formals-to-names is a list of names.
;   3. The body of the expression in a let-expression is unchanged in a lambda-expression.
; The result of let-formals-to-values is the list of args.


; *let - Evaluates an expression with a table and returns the result. 
(define (*let e table)
  (myapply
   (meaning
    (list 'lambda (let-formals-to-names (second e)) (third e))
    table)
   (evlis
    (let-formals-to-values (second e))
    table)
   )
  )


; list-to-action is updated with *let. Its position does not matter inside the cond statement.
; list-to-action - Checks the expression and returns a function that can be used to evaluate the expression.
(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond 
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote cond))
          *cond)
         ((eq? (car e) (quote let))
          *let)
         (else *application)))
      (else *application))))





; TESTS
(value '(let ((x 5)) (add1 x)))
(value '(let ( (x 5) (y 4)) (cond (else (add1 x)))))
(value '(cond ((zero? (add1 ((lambda (x) (add1 x)) -2))) ((lambda (x) ((lambda (y) (add1 y)) x)) 5))))

; We provide some tests showing how lambda can be mixed with let.
(value '((lambda (x) ((lambda (y) (add1 y)) x)) 5)) ; lambda (lambda)
(value '((lambda (x) (let ((y x)) (add1 y))) 5))    ; lambda (let)
(value '(let ((x 5)) ((lambda (y) (add1 y)) x)))    ; let (lambda)
(value '(let ((x 5)) (let ((y x)) (add1 y))))       ; let (let)

