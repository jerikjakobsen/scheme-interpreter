; Add1
(define (add1 x) (+ x 1))

; Sub1
(define (sub1 x) (- x 1))

; Atomic element
(define (atom? x) (and (not (pair? x)) (not (null? x))))

; First element of list.
(define first car)

; Second element of list.
(define second cadr)

; Third element of list.
(define third caddr)

; Build - Takes two atoms and returns a list.
(define (build s1 s2) (list s1 s2))




; Entries - key, value pairs.

; throw - Halts execution and shows an error message.
(define (throw e) (begin (display "Error not found: ") (display e) (car '())))

; new-entry - Creates an associative array of variables to values.
(define new-entry build)

; names - Returns a list of names from an entry.
(define names first)

; values - Returns a list of values from an entry.
(define vals second)

; lookup-in-entry - Takes a name and returns its corresponding value.
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

; lookup-in-entry-help - Recursively checks the head of the list until it matches name.
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names)
       (entry-f name))
      ((eq? (car names) name)
       (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

; TESTS
(define entry-test (new-entry '(appetizer entree beverage) '(pate boeuf vin)))
;(lookup-in-entry 'appetizer entry-test throw)




; Tables - lists of entries.

; initial-table - The table to be extended with additional entries.
(define initial-table '())

; extend-table - Places a new entry at the front of a table.
(define extend-table cons)

; table-f - Halts execution and shows an error message.
(define (table-f e) (begin (display "Error not found: ") (display e) (car '())))

; lookup-in-table - Takes a name, table and returns the value associated with the name.
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

; *identifier - Takes a name, table and returns the value associated with the name.
; Compared to the above function, this one has the error function already bound.
(define *identifier
  (lambda (e table)
    (lookup-in-table e table table-f)))

; TESTS
(define table-test
  (extend-table (new-entry '(appetizer entree beverage) '(pate boeuf vin))
   (extend-table (new-entry '(beverage dessert) '((food is) (number one with us)))
                 initial-table)))

;(lookup-in-table 'dessert table-test table-f)
;(*identifier 'appetizer table-test)




; Primitives - A 2-member list where the first element is the string "primitive", or "non-primitive"

; primitive? - Checks for the "primitive" key.
(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

; primitive? - Checks for the "non-primitive" key.
(define non-primitive? 
  (lambda (l) 
    (eq? (first l) (quote non-primitive))))

; *const - Dereferences booleans and numbers from their string representation. Otherwise constructs a ('primitive 'string) association.
; Note that this function requires a table argument to maintain arity-2 for compatibility reasons.
(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

; TESTS
;(*const '#t table-test)  ; #t
;(*const '1 table-test)   ; 1
;(*const 'car table-test) ; (primitive car)




; Actions - These are functions which take a single argument and return a function of arity-2.

; atom-to-action - Checks if an atom is a const or an identifier and returns those functions respectively.
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
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote square)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier)))) ; symbols are of type identifier.

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

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))



; Note that some of the above functions are not fully defined, in the sense that some routes cannot be fully evaluated.

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


; TESTS
;(value '1)
;(value '#t)
;(value 'sub1)
; TESTS
;(atom-to-action #f)


; Expressions

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
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ((eq? name (quote square))
       (* (first vals) (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))

; TESTS


(define myapply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (myapply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (myapply-closure
        (second fun) vals)))))

(value 'square)
(myapply-primitive 'square '(8)) ;; 64
(myapply '(primitive square) '(8)) ;; 64
(myapply (value 'square) '(8)) ;; 64

(define *application 
  (lambda (e table) 
    (myapply 
      (meaning (function-of e) table) 
      (evlis (arguments-of e) table)))) 



(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define *quote
  (lambda (e table)
    (text-of e)))


(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))


