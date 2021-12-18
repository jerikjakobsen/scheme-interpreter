; Problem 2
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
       (sub1 (first vals)))
;;;; deliberate error: ask class to figure out how to repair it.  
      
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



































; Pre condition: new-entry accepts names and vals
; where names is a list of names (with no duplicates) and vals is a list of s-expressions of the same length as names
; Post condition: An entry is returned with names associated to its corresponding values

(define (new-entry names values)
  (cond ( (null? names) names)
        ( else (cons (cons (car names) (car values)) (new-entry (cdr names) (cdr values))))))
;idea
;In order to build a list of association pairs from names and values, we can do so inductively,
;if we have (new-entry (cdr names) (cdr values)) then we can use this to build (new-entry names values)
;by consing the association pair of the (car names) (car values) to (new-entry (cdr names) (cdr values)).
;Our base case will be when names is the empty list, (since by our pre condition names and values must have the same length,
;values is also empty) we return the empty list.
;
;Proof via weak induction
;
;For this proof we will be inducting on N + V where,
;N = length of names
;V = length of values,
;both N and V are non-negative integers.
;
;Induction Hypothesis
;
;Let A(N + V) be the property that new-entry works with names of length N and values of length V.
;Assume A( (N-1) + (V-1) ) holds, that is A(N + V - 2) holds.
;
;Base Case
;
;A(0) holds, since when both N and V are 0, we return the empty entry (ie the empty list)
;
;Induction Step
;
;Since by our assumption A( (N-1) + (V-1) ) holds, (new-entry (cdr names) (cdr values)) returns the correct result.
;We can get (new-entry names values) by consing the association pair of the first element of names with the first element
;of values to (new-entry (cdr names) (cdr values)). The association pair can be formed with (cons (car names) (car values))

;Termination
;Since N = V, N + V is of the form 2k, where k is an integer greater than 0. By subtracting 2 from a positive even integer
;repeatedly we will eventually reach 0, our base case, and terminate.

                      
(define lookup-in-entry (lambda (name entry entry-f)
                               (cond ( (null-entry? entry) (entry-f name))
                                     ( (eq? (car-entry-name entry) name) (car-entry-value entry))
                                     ( else (lookup-in-entry name (cdr-entry entry) entry-f)))))

;Proof that changes made to lookup-in-entry satisfies the specs of the original sub system
;The original lookup-in-entry accepts the name to be looked for, the entry to be searched through and the
;function to be run if no match is found. The original preconditions for lookup-in-entry are
;1) name is a non-numeric atom
;2) entry is well formed entry
;
;What is a well formed entry?
;
;A well formed entry is a data structure that supports these operations,
;1) car-entry-name of the entry should return the name of the first association in the entry
;2) car-entry-value of the entry should return the value of the first association in the entry
;3) cdr-entry of the entry should return a list with all associations but the first association
;4) null-entry? should return #t if the entry is empty and #f if it is not empty
;5) (new-entry names values) should return an entry with names and values associated to each other

(define car-entry-name (lambda (x) (car (car x))))
(define car-entry-value (lambda (x) (cdr (car x))))
(define cdr-entry cdr)
(define null-entry? null?)

; Tests
(value '( (lambda (x y) (cons x ((lambda (x) (cons x (quote ()))) y))) 1 2 ) )
(value '( (lambda (x y) (eq? x ((lambda (x) (add1 x)) y))) 2 1 ) )



; For every pair of (name . value) inside a list:
; car to get the name,
; cdr to get the value.

; We use map and apply the function car, and another map to apply the function cdr.
; This is a weak induction on the length of the list.
; In this case, both car and cdr turn a list [e] -> e into an atom.
; Because the outer list is preserved by the map function, we can safely say that map returns a list.

; names - Takes a entry of ((name values) ...) and returns a list of names.
(define (names entry)
  (map (lambda (x) (car x)) entry))

; vals - Takes a list of ((name values) ...) and returns a list of values.
(define (vals entry)
  (map (lambda (x) (cdr x)) entry))

; TESTS

(names (new-entry '(a b c) '(1 2 3)))
(vals (new-entry '(a b c) '(1 2 3)))