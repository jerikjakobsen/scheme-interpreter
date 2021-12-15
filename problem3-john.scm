(define (atom? a)
  (and (not (pair? a)) (not (null? a))))

(define (sexp? exp)
  (cond ( (atom? exp) #t)
        ( (null? exp) #t)
        ( (list? exp) (and-map exp sexp?))
        (else #f)))

(define (and-map lst f)
  (cond ( (null? lst) #t)
        ( else (and (f (car lst)) (and-map (cdr lst) f)))))
;
;idea for and-map
;We can apply f to the car of lst, then logically and the result of that with the (and-map (cdr lst) f). Essentially cdring through the list
;and anding all the elements with f applied to them.
;
;Pre-conditions:
;1) f accepts one argument
;2) f returns a boolean
;3) f terminates
;4) lst must be a list
;Post-conditions
;The logical and of all (f ei), where ei is the ith element in the provided lst, is returned.
;
;Proof by weak induction
;
;Induction Hypothesis
;
;Let A(k) be the property that (and-map lst f) returns the correct result for lst, where lst is of length k. (k is a nonnegative integer)
;Assume A(k-1) holds.
;
;Base Case
;A(0) in this case there are no elements to apply f to so we can return true.
;
;Induction Step
;Since by our induction hypothesis A(k-1) holds, (and-map (cdr lst) f) works, so we can get (and-map lst f) by anding
;(and-map (cdr lst) f) with (f (car lst)).

;Check type of expression methods

(define (primitive-function? exp)
  (cond
    ((eq? (car exp) 'cons) #t)
    ((eq? (car exp) 'car) #t)
    ((eq? (car exp) 'cdr) #t)
    ((eq? (car exp) 'null?) #t)
    ((eq? (car exp) 'eq?) #t)
    ((eq? (car exp) 'atom?) #t)
    ((eq? (car exp) 'zero?) #t)
    ((eq? (car exp) 'add1) #t)
    ((eq? (car exp) 'sub1) #t)
    ((eq? (car exp) 'number?) #t)
    (else #f)))

(define (non-primitive-function? exp)
  (cond ( (not (list? (car exp))) #f)
        ( (not (equal? 3 (length (car exp)))) #f)
        ( else (lambda? (car exp)))))

(define (cond? exp) (eq? 'cond (car exp)))

(define (quote? exp) (eq? 'quote (car exp)))

(define (lambda? exp) (eq? 'lambda (car exp)))

;Check cond methods

(define (cond-check-helper lst)
  (cond ( (null? lst) #t)
        ( (not (list? (car lst))) #f)
        ( (not (equal? 2 (length (car lst)))) #f)
        ( else (and
                (simple-check (car (car lst)))
                (simple-check (car (cdr (car lst))))
                (cond-check-helper (cdr lst))
               )
        )
 ))

(define (check-cond exp)
  ( cond ( (< (length exp) 2) #f)
         ( else (cond-check-helper (cdr exp)))))

(define (check-quote exp)
  (cond ( (not (equal? (length exp) 2) ) #f)
        ( else (sexp? (car (cdr exp))))))

;Check non-primitive function methods

(define (check-lambda lambda)
  (cond ( (not (list? (second lambda))) #f)
        ( (not (list? (third lambda))) #f)
        ( else (and (check-formals lambda) (check-body lambda)))))
(define (check-non-primitive-function exp)
  (and (check-lambda (car exp)) (formals-match-arguments exp) (check-function-args (cdr exp))))

(define (second lst) (car (cdr lst)))
(define (third lst) (car (cdr (cdr lst))))

(define (check-formals lambda)
  (check-formals-helper (second lambda)))

(define (check-formals-helper formals)
  (and-map formals (lambda (x) (and (atom? x) (not (number? x))))))

(define (check-body lambda)
  (simple-check (third lambda)))
(define (formals-match-arguments exp)
  (equal? (length (second (car exp))) (length (cdr exp))))
(define (check-function-args args)
  (and-map args simple-check))



(define (simple-check exp)
  (cond ( (not (sexp? exp)) #f)
        ( (atom? exp) #t)
        ( (> (length exp) 0)
          (cond ( (primitive-function? exp) (check-function-args (cdr exp)))
                ( (cond? exp) (check-cond exp))
                ( (quote? exp) (check-quote exp))
                ( (non-primitive-function? exp) (check-non-primitive-function exp))
                (else #f)
          ))
        (else #f)))

(define c '(cond ((zero? 1) (add1 1)) ) )
(define q '(quote (h p)) )
(define p '(cons (quote a) (quote b)))
(define np '( (lambda (x y) (cons x y)) 3 (quote a)))

(simple-check c)
(simple-check q)
(simple-check p)
(simple-check np)