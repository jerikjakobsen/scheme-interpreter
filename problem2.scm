(define (atom? a)
  (and (not (pair? a)) (not (null? a))))

(define build (lambda (sl s2)
                (cond
                  (else (cons sl (cons s2 (quote ())))))))

(define first (lambda (p) (car p)))
(define second (lambda (p) (car (cdr p))))
(define third (lambda (p) (car (cdr (cdr p)))))

; Pre condtion: new-entry accepts names and vals
; where names is a list of names (with no duplicates) and vals is a list of s-expressions of the same length as names
; Post condtion: An entry is returned with names associated to its corresponding values

(define (new-entry names values)
  (cond ( (null? names) names)
        ( else (cons (cons (car names) (car values)) (new-entry (cdr names) (cdr values))))))
;idea
;In order to build a list of association pairs from names and values, we can do so inductively,
;if we (new-entry (cdr names) (cdr values)) then we can use this to build (new-entry names values)
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
                               (cond ( (null? entry) (entry-f name))
                                     ( (eq? (car (car entry)) name) (cdr (car entry)))
                                     ( else (lookup-in-entry name (cdr entry) entry-f)))))

(define extend-table cons)

(define lookup-in-table (lambda (name table table-f)
                          (cond ( (null? table) (table-f name))
                                ( else (lookup-in-entry name (car table) (lambda (name) (lookup-in-table name (cdr table) table-f)))))))

(define expression-to-action (lambda (e)
(cond
  ( (atom? e) (atom-to-action e))
  ( else (list-to-action e)))))

(define atom-to-action (lambda (e)
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
  ((eq? e (quote sub1)) *const)
  ((eq? e (quote number?)) *const)
  (else *identifier))))

(define list-to-action (lambda (e)
                         (cond
                           ((atom? (car e))
                            (cond ((eq? (car e) (quote quote)) *quote)
                                  ((eq? (car e) (quote lambda)) *lambda)
                                  ((eq? (car e) (quote cond)) *cond)
                                  (else *application)))
                           (else *application))))

(define value (lambda (e)
                (meaning e (quote ()))))

(define meaning (lambda (e table)
                  ((expression-to-action e) e table)))

(define *const (lambda (e table)
                 (cond
                   ((number? e) e)
                   ((eq? e #t) #t)
                   ((eq? e #f) #f)
                   (else (build (quote primitive) e)))))

(define *quote (lambda (e table)
                 (text-of e)))

(define text-of second)

(define *identifier (lambda (e table)
                      (lookup-in-table e table initial-table)))

(define initial-table (lambda (name)
                        (car (quote ()))))

(define *lambda (lambda (e table)
                  (build (quote non-primitive) (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (car lines))
                                                  table ) )
      ((meaning (question-of (car lines))
                table )
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))

(define else? (lambda (x)
                (cond
                  ((atom? x) (eq? x (quote else))) (else #f))))

(define question-of first)

(define answer-of second)

(define *cond (lambda (e table)
                (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ())) (else
                                 (cons (meaning (car args) table) (evlis (cdr args) table))))))

(define *application (lambda (e table)
                       (apply-tls
                         (meaning (function-of e) table) (evlis (arguments-of e) table))))


(define function-of car)

(define arguments-of cdr)

(define primitive? (lambda (l)
                     (eq? (first l) (quote primitive))))

(define non-primitive? (lambda (l)
                         (eq? (first l) (quote non-primitive))))

(define apply-tls (lambda (fun vals)
                 (cond
                   ((primitive? fun) ( apply-primitive (second fun) vals))
                   ((non-primitive? fun) (apply-closure (second fun) vals)))))

(define apply-primitive (lambda (name vals)
                          (cond
                            ((eq? name (quote cons)) (cons (first vals) (second vals)))
                            ((eq? name (quote car)) (car (first vals)))
                            ((eq? name (quote cdr)) (cdr (first vals)))
                            ((eq? name (quote null?)) (null? (first vals)))
                            ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
                            ((eq? name (quote zero?)) (zero? (first vals)))
                            ((eq? name (quote add1)) (+ 1 (first vals)))
                            ((eq? name (quote sub1)) (- (first vals) 1))
                            ((eq? name (quote atom?)) (atom? (first vals)))
                            ((eq? name (quote number?)) (number? (first vals))))))

(define :atom? (lambda (x)
                  (cond ( (atom? x) #t)
                        ( (null? x) #f)
                        ( (eq? (car x) (quote primitive)) #t)
                        ( (eq? (car x) (quote non-primitive)) #t)
                        (else #f))))

(define apply-closure (lambda (closure vals)
                        (meaning (body-of closure) (extend-table
                                                    (new-entry (formals-of closure) vals )
                                                    (table-of closure)))))

; Tests
(value '( (lambda (x y) (cons x ((lambda (x) (cons x (quote ()))) y))) 1 2 ) )
(value '( (lambda (x y) (eq? x ((lambda (x) (add1 x)) y))) 2 1 ) )