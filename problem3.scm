; Problem 3
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

































; Solution to Problem 3. By Edison Hua and John Jakobsen.

; We propose the following simple solution:
; A list of function-names that is defined by the TLS checker.
; A TLS-expression consisiting of booleans, numbers, and expressions.

; Inductive Types
; function-name = cons | car | cdr | null? | eq? | atom? | zero? | add1 | mul | sub1 | number?
; tls-expression = #t | #f | number | (function-name tls-expression ... tls-expression)


; #t | #f | number

; bool-or-number? - Takes an atom and checks if it is a boolean or a number. Returns a boolean.
(define (bool-or-number? name)
  (cond
    ((eq? name #t) #t)
    ((eq? name #f) #t)
    ((number? name) #t)
    (else #f)))


; (function-name tls-expression ... tls-expression)

; primitive-function? - Takes an expression and checks the function-name and arity. Returns a boolean.
(define (primitive-function? exp)
  (let ((name (car exp)) (arity (length (cdr exp))))
    (cond
      ((and (= arity 2) (eq? name 'cons))  #t)
      ((and (= arity 1) (eq? name 'car)) #t)
      ((and (= arity 1) (eq? name 'cdr)) #t)
      ((and (= arity 1) (eq? name 'null?)) #t)
      ((and (= arity 2) (eq? name 'eq?)) #t)
      ((and (= arity 1) (eq? name 'atom?)) #t)
      ((and (= arity 1) (eq? name 'zero?)) #t)
      ((and (= arity 1) (eq? name 'add1)) #t)
      ((and (= arity 1) (eq? name 'sub1)) #t)
      ((and (= arity 1) (eq? name 'number?)) #t)
      (else #f))))



; idea for and-map
; We can apply f to the car of lst, then logically and the result of that with the (and-map (cdr lst) f). Essentially cdring through the list
; and anding all the elements with f applied to them.

; Pre-conditions:
; 1) f accepts one argument
; 2) f returns a boolean
; 3) f terminates
; 4) lst must be a list
; Post-conditions
; The logical and of all (f ei), where ei is the ith element in the provided lst, is returned.
;
; Proof by weak induction
;
; Induction Hypothesis
;
; Let A(k) be the property that (and-map lst f) returns the correct result for lst, where lst is of length k. (k is a nonnegative integer)
; Assume A(k-1) holds.
;
; Base Case
; A(0) in this case there are no elements to apply f to so we can return true.
;
; Induction Step
; Since by our induction hypothesis A(k-1) holds, (and-map (cdr lst) f) works, so we can get (and-map lst f) by anding
; (and-map (cdr lst) f) with (f (car lst)).

(define (and-map lst f)
  (cond ((null? lst) #t)
        (else (and (f (car lst)) (and-map (cdr lst) f)))))



; check-function-args - Takes a list and returns a boolean.
; Note that this function is meant to be called using the cdr of a list, as this checks function arguments.
; Simple-check is applied to each argument using and-map.
(define (check-function-args args)
  (and-map args simple-check))


; #1 - Sketch of simple-check
; pre-condition: accepts an expression.
; post-condition: returns a boolean.

; This is a recursive function that will apply check-function-args/and-map to lists.
; atom -> bool-or-number?
; null -> false
; list -> primitive-function? -> check-function-args
; Terminates when it fails one of the checks, or all atoms have been verified to be bool-or-number.
; Preserves passing the remainder of the list to check-function-args.
; If any nested arguments exist, and-map applies simple-check to each of them.
; weak-enough to allow nested lists to pass, and strong enough to filter out atoms.
; Each element of the expression is evaluated to either #t or #f by simple-check,
; and the and-map will "and" all lists of booleans to produce either a true or false.
; Every nested list becomes a boolean, so the final result describes whether the entire statement is syntactically correct.
(define (simple-check exp)
  (cond ( (null? exp) #f)
        ( (bool-or-number? exp) #t)
        ( else (and (primitive-function? exp) (check-function-args (cdr exp))))))

; TESTS
;(simple-check '())                      ; fail
;(simple-check '35465465)                ; pass
;(simple-check '(add1 4))                ; pass
;(simple-check '(add1 (sub1 7)))         ; pass
;(simple-check '(eq? (sub1 7) (add1 5))) ; pass









; #2 - Add quote

(define (quote? exp) (eq? 'quote (car exp)))

; sexp? - checks if the expression is a valid symbolic expression.
; The termination condition is when it reaches an an atom? null? or pair?.
; The first call always works because of the else clause.
; and-map is used to recursively apply sexp? to every element of a nested list.
; This action preserves the booleans, as and-map will and every bool, and return a unnested boolean.
; Therefore, every nested list will be reduced to a top level boolean.
; Therefore, sexp? is strong enough to return a result of either #t or #f.
; Moreover, sexp? is weak enough to pass nested lists to sexp? again.
(define (sexp? exp)
  (cond ( (atom? exp) #t)
        ( (null? exp) #t)
        ( (list? exp) (and-map exp sexp?))
        (else #f)))

; check-quote - Takes an expression with the first element being quote, and returns a boolean.
; Ensures that quote is arity-1.
; Verifies that the argument is an symbolic expression.
(define (check-quote exp)
  (cond ( (not (equal? (length exp) 2) ) #f)
        ( else (sexp? (car (cdr exp))))))


(define (simple-check exp)
  (cond ( (null? exp) #f)
        ( (bool-or-number? exp) #t)
        ( (quote? exp) (check-quote exp))
        ( else (and (primitive-function? exp) (check-function-args (cdr exp))))))

; TESTS
;(simple-check '(quote (h p)))              ; pass
;(simple-check '(quote ()))                 ; pass
;(simple-check '(cons (quote a) (quote b))) ; pass









; #3 - Add cond
(define (cond? exp) (eq? 'cond (car exp)))


; check-cond - Takes an expression that begins with cond and returns a boolean.
; verifies that arity is greater than 1.
(define (check-cond exp)
  ( cond ( (< (length exp) 2) #f)
         ( else (cond-check-helper (cdr exp)))))

; cond-check-helper - Takes a list and returns a boolean.
; A cond cell is a list with the first element the question and the second element the answer
; idea: A cond statement has multiple cells. Every cond cell has two arguments.
; check if the car of the list is a list. Make sure its length is 2.
; Pre-Condition: There is at least one cond cell
; Post-Condition returns true if all cond cells are in proper form
; Recursively cdr down the list. This is a weak induction over the length of the list.
; Induction hypothesis: Assume cond-check-helper works for lists of k - 1
; Induction Step: we can and the simple-check for the question and answer of the cond-line with ( cond-check-helper (cdr lst)), this works by our hypothesis
; base case is the if the list is null
; termination argument - The list is finite and will eventually become null through cdring through the whole list
; Results are preserved using and.

; Call simple-check on the two elements of the cond cell to verify those as well.
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

;Structural Induction For Checking cond
;
;Assume the simple-check works for the sub-components of cond, that is the question and answer parts of the cond cell works with simple-check.
;Base case: there are no cond cells, so we return false since cond requires at least one cond cell.
;Induction step: Assuming simple-check works for the cond cells, all we must do is verify the structure of the cond statement.

(define (simple-check exp)
  (cond ( (null? exp) #f)
        ( (bool-or-number? exp) #t)
        ( (quote? exp) (check-quote exp))
        ( (cond? exp) (check-cond exp))
        ( else (and (primitive-function? exp) (check-function-args (cdr exp))))))

; TESTS
;(simple-check '(cond ((zero? 1) (add1 1)))) ; pass
;(simple-check '(cond () ())) ; fail

;(simple-check '(cond ((zero? 4) 5) ((eq? (add1 7) (sub1 10)) #t) (#t #f))) ; pass







; #4 - Add identifiers.

; check-identifier - Takes a name and a table, and returns a boolean.
(define (check-identifier name table)
  (check-table table name))

;Pre-condition: name is an atom and table is a well formed table
;Post-condition: returns true if the name is in the table and false if it isnt
(define (check-table table name)
  (cond ( (null? table) #f)
        ( (check-entry (names (car table)) name) #t)
        ( else (check-table (cdr table) name))))
;Proof for check-table
;
;Idea: Cdr down the table and check each entry for the value inquired, if the value is in an entry return true, otherwise
;keep looking in the other entries
;
;Proof by induction on the length of the table
;Assume check-table works for tables of size k-1, where k is the number of entries in table.
;Base case: When the table is null the value is definitely not in the table, so we return false.
;Induction Step:
;To get check-table of table where it is of size k, we can check if the entry contains the value, if it does we can return true, if it doesnt,
;then since (check-table (cdr table) name) returns the correct answer, we can return that.
;Termination Argument:
;Since k is an integer, and we are decrementing it finitely, we will eventually hit 0 the base case.

;Pre-condition: name is an atom and entry-names is a list of atoms	
;Post-condition: returns true if the name is in the entry-names
(define (check-entry entry-names name)
  (cond ( (null? entry-names) #f)
        ( (eq? (car entry-names) name) #t)
        ( else (check-entry (cdr entry-names) name))))
;The proof is similar to above, instead of cdring down the table, we are cdring down the names of the entry and returning true
;if the car of the names of the entry is equal to the provided name, and false if the entry is null.


; CHANGE simple-check TO ACCEPT A TABLE OF IDENTIFIERS.
(define (check-function-args args table)
  (and-map args (lambda (x) (simple-check x table)))) ; USE A LAMBDA TO BIND ARGUMENTS.

(define (check-cond exp table)
  ( cond ( (< (length exp) 2) #f)
         ( else (cond-check-helper (cdr exp) table))))

(define (cond-check-helper lst table)
  (cond ( (null? lst) #t)
        ( (not (list? (car lst))) #f)
        ( (not (equal? 2 (length (car lst)))) #f)
        ( else (and
                (simple-check (car (car lst)) table)
                (simple-check (car (cdr (car lst))) table)
                (cond-check-helper (cdr lst) table)
               )
        )
 ))


(define (simple-check exp table)
  (cond ( (null? exp) #f)
        ( (bool-or-number? exp) #t)
        ( (atom? exp) (check-identifier exp table))
        ( (quote? exp) (check-quote exp))
        ( (cond? exp) (check-cond exp table))
        ( else (and (primitive-function? exp) (check-function-args (cdr exp) table)))))


; TESTS

(define table-test
  (extend-table (new-entry '(appetizer entree beverage) '(pate boeuf vin))
   (extend-table (new-entry '(beverage dessert) '((food is) (number one with us)))
                 '())))


;(check-entry (names (car table-test)) 'b)
;(check-table table-test 'entree)
;(meaning 'appetizer table-test)
;(check-identifier 'appetizer table-test)
;(check-identifier 'no table-test)

;(simple-check '(add1 appetizer) table-test)                   ; pass
;(simple-check '(add1 sushi) table-test)                       ; fail
;(simple-check '(cond ((zero? 1) (add1 beverage))) table-test) ; pass
;(simple-check '(cond () ()) table-test)                       ; fail




















; #5 - Add lambda.
; idea: to verify the correctness, check for the length of a lambda expression = 3.
; Match the number of formal arguments to number of applied arguments.
; Add the formals to the environment, and pass the new-table to simple-check.
; Check the arguments for proper tls form

; Because a lambda can also be of the form ((lambda x 5) 7)
; We must accept atoms as well as lists.

;Structural Induction For checking lambda
;
;Assume the simple-check works for the sub-components of lambda, that is the body and arguments to lambda works with simple-check.
;Base case: The lambda is malformed, we return false.
;Induction step: Assuming simple-check works for the cond cells, all we must do is verify the structure of the cond statement.

; lambda? - Checks for lambda format and returns a boolean.
(define (lambda? lambda)
  (cond ( (not (list? lambda)) #f)
        ( (not (equal? 3 (length lambda))) #f)
        ( else (eq? 'lambda (car lambda)))))

; safe-length - Returns 1 for atoms, sizes lists.
(define (safe-length s) (if (atom? s) 1 (length s)))

; check-formals - Use an and-map to ensure the formals are atoms and not numbers.
(define (check-formals formals)
  (let ((atom-or-number? (lambda (x) (and (atom? x) (not (number? x))))))
  (if (atom? formals)
      (atom-or-number? formals)
      (and-map formals atom-or-number?))))

; formals-match-arguments - Checks if the # of lambda arguments matches the # of applied arguments.
(define (formals-match-arguments exp)
  (equal? (safe-length (second (car exp))) (safe-length (cdr exp))))

; add-formals-to-table - Extends a table with the formals.
(define (add-formals-to-table formals table)
  (extend-table (formals-entries formals) table))

; formals-entries - Creates an entry of from the formals of the lambda form.
; Note that a dummy value of #t is used. The main purpose of the syntax checker is to check syntax,
; not to evaluate. Therefore a dummy value is sufficient for this purpose.
(define (formals-entries formals)
  (if (atom? formals)
      (new-entry (list formals) (generate-list #t (safe-length formals)))
      (new-entry formals (generate-list #t (safe-length formals)))))

; generate-list - Takes an element and a size, and returns a list of length size.
; recursive function. termination: when size is equal to 0.
; strong-enough? when size is 0 the null list is returned.
; preserves? Cons the null list builds up a list.
; weak-enough? size is decremented until 0.
(define (generate-list element size)
  (if (> size 0)
      (cons element (generate-list element (- size 1)))
      '()))

; check-lambda - Returns true if this is a proper lambda.
(define (check-lambda lambda table)
  (let ((formals (second lambda))
        (body (third lambda)))
    (and
     (check-formals formals)
     (simple-check body (add-formals-to-table formals table)) ; This line adds the lambda variables to the table.
     )))




; non-primitive-function? - Checks for applied lambda format and returns a boolean.
(define (applied-lambda? exp) (lambda? (car exp)))

; check-non-primitive-function - Returns true if this is a proper applied lambda.
(define (check-applied-lambda exp table)
  (let ((lambda (car exp))
        (formals (second (car exp)))
        (body (third (car exp))))
    (and
     (check-formals formals)
     (formals-match-arguments exp)
     (simple-check body (add-formals-to-table formals table)) ; This line adds the lambda variables to the table.
     (check-function-args (cdr exp) table)
     )))





; TESTS
;(define q '((lambda x 5) 2))
;(define q '((lambda () (sub1 3))))
;(define q '((lambda (x) (sub1 x)) 2))
;(define q '((lambda x (sub1 5)) 2))
;(define q '((lambda x (sub1 x)) 2) )

;(value q)
;(car q)                                     ; lambda
;(second (car q))                            ; formals
;(third (car q))                             ; body
;(add-formals-to-table (second (car q)) '()) ; new environment
;(check-formals (second (car q)))
;(simple-check (third (car q))
;              (add-formals-to-table (second (car q)) '()))
;(formals-match-arguments q)
;(check-applied-lambda q '())


; Note that this properly checks for closures!!!
; Here the value z is not in the formals.
;(define q '((lambda (x) (sub1 z)) 2))
;(check-non-primitive-function q '())

; ofc if z is defined...
;(define q '((lambda (z) (sub1 z)) 2))
;(check-non-primitive-function q '())


;;;;;;;;;;;;;;;;;;;
;; FINAL VERSION ;;
;;;;;;;;;;;;;;;;;;;
(define (simple-check exp table)
  (cond ( (null? exp) #f)
        ( (bool-or-number? exp) #t)
        ( (atom? exp) (check-identifier exp table))
        ( (quote? exp) (check-quote exp))
        ( (cond? exp) (check-cond exp table))
        ( (lambda? exp) (check-lambda exp table))
        ( (applied-lambda? exp) (check-applied-lambda exp table))
        ( else (and (primitive-function? exp) (check-function-args (cdr exp) table)))))



(define (check exp) (simple-check exp '()))


; TESTS
; pass

; pass
;(check '(lambda x 10))
;(check '(lambda (x) (add1 x)))
;(check '(lambda (x y) (cons x y)))
;(check '(lambda (z) ((lambda (x y) (cons x y)) 3 (quote a))))

;(check '((lambda x 10) 5))
;(check '((lambda (x) (add1 x)) 5))
;(check '((lambda (x y) (cons x y)) 3 (quote a)))
;(check '((lambda (z) ((lambda (x y) (cons x y)) 3 (quote a))) (quote 15)))

; fail
;(check '())                ; null is not an expression.
;(check '(add2 7))          ; add2 is not a function.
;(check '(add1 (sub1 0 0))) ; sub1 is arity-1.
;(check '(eq? (sub1 0)))    ; eq? is arity-2.




; We found a bug in the tls-interpreter.

;(value '((lambda x x) 2) )
; Passing a lambda with no parenthesis around the parameters i.e (lambda x 5) creates a corrupted table.
; We have fixed our syntax checker to not create these errors by checking if the formal is an atom, instead of assuming it is a list.
(check '((lambda x x) 2) )




