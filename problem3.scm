;3.  Write, and prove correct, a syntax checker for TLS-Scheme.  Use the inductive definition of TLS-Scheme
;    given in class.  You will need to give a complete specification for your program, which should
;    be written in R5RS.
;
;    I suggest starting with a simpler program - let's call it simple-check - which does not check for
;    arity correctness of applications or unbound variables.  When you understand how to do this, then
;    extend your solution to reject programs which either
;
;       a. contain unbound symbols, or
;       b. contain applications in which the number of arguments in some phrase (f x1 ... xn) does not
;          match the number of formal parameters given in the definition of f
;
;
;    Thus the occurrence of a symbol, say x, in an expression when there is no wrapping occurrence of
;    (lambda (x...) ...), would be cause your program to reject the input, and any occurrence of the
;    kind
;
;       ((lambda (x y) ... ) p), or 
;
;       ((lambda (f x) ... (f arg) ...  )
;          (lambda (x y) ...)
;          val-for-x)
;
;(in which f, bound to a closure, is applied to a number of arguments inconsistent with the closure)
;would also cause your program to reject the input.


; Can be a number, #t, or #f.
; Can be nested.
; Must be able to check for the starting element of a list - map cannot be used!

; Examples:
; (add1 5)
; ((sub1 7)

;function-name = cons | car | cdr | null? | eq? | atom? | zero? | add1 | sub1 | number?
;tls-expression = #t | #f | number | (function-name tls-expression ... tls-expression)

; Check for bool-or-number? - Returns 0 on success, and 1 on failure.
(define (bool-or-number? name)
  (cond
    ((eq? name '#t) 0)
    ((eq? name '#f) 0)
    ((number? name) 0)
    (else 1)))

;(bool-or-number? '15)
;(bool-or-number? '#f)

; Check for function-name - Returns 0 on success, and 1 on failure.
(define (function-name? name)
  (cond
    ((eq? name 'cons) 0)
    ((eq? name 'car) 0)
    ((eq? name 'cdr) 0)
    ((eq? name 'null?) 0)
    ((eq? name 'eq?) 0)
    ((eq? name 'atom?) 0)
    ((eq? name 'zero?) 0)
    ((eq? name 'add1) 0)
    ((eq? name 'sub1) 0)
    ((eq? name 'number?) 0)
    (else 1)))

;(function-name? 'cons)
;(function-name? '7)

; Q: Why use numbers instead of #t and #f?
; A: Because the following code uses apply. And is a short-circuit operator and will not play nice with apply. Instead + is used.

; Takes a simple expression and returns a positive number if the syntax is wrong, and 0 upon success.

; The idea is to apply a list of elements to bool-or-number.
; When all the values become either 0 or 1, the addition operator will count up the errors.
; bool-or-number -> 0
; The function terminates when there are no nested lists in the arguments.
; The first call checks for an bool-or-number, or a list.
; If it is a list, then it is decomposed into two parts.

; From our inductive definition.
; (function-name tls-expression ... tls-expression)
; the car is passed to function-name?
; the cdr is preserved recursively.

; 1. Check all atoms for bool-or-number and replace with 0 or 1. 
; 2. The number of errors is preserved by summing the list.
; 3. On the next call, all values are numeric, and is therefore strong enough to be passed back to apply +.

; Note the deferred calls are apply +, and map.
; apply + coalesces the function-name and the remainder of the list.
; map is not directly called on the elements of the list. Therefore it preserves.
; Instead, map applies bool-or-number? to each element of the list.
; Remember the first element is processed separately!
; Not pair? is strong enough to prove that all elements sent to bool-or-number are either the null list or an atom.
; And pair? is weak enough to allow nested lists to pass through.

; Induction is done via map, which inducts (weakly) over the length of the list.

; Also note that it correctly handles numeric values inside lists.
; When deffering calls, all nested lists and values are preserved.
; bool-or-number?, reduces all atoms and the null list to either 1 or 0.
; Then as the stack coalesces, there are only 0's and 1's representing errors in the syntax.
; Finally + is variadic, so it successfully reduces a list of numbers to a number.

; This function returns a positive number if the syntax is wrong, and 0 upon success.
(define (count tree)
  (if (pair? tree)
      (if (eq? (car tree) 'cond)
          (check-cond tree) ; lol ur function goes here.
          (apply + (function-name? (car tree)) (map count (cdr tree))))
      (bool-or-number? tree)))

(count '(add1 5 9))
(count '(atom? (sub1 6)))










; Extend to functions of specific arity.


; Check for function-name - Returns 0 on success, and 1 on failure.
(define (function-name? ls)
  (let ((name (car ls)) (arity (- (length ls) 1)))
    (cond
      ((and (= arity 2) (eq? name 'cons))  0)
      ((and (= arity 1) (eq? name 'car)) 0)
      ((and (= arity 1) (eq? name 'cdr)) 0)
      ((and (= arity 1) (eq? name 'null?)) 0)
      ((and (= arity 2) (eq? name 'eq?)) 0)
      ((and (= arity 1) (eq? name 'atom?)) 0)
      ((and (= arity 1) (eq? name 'zero?)) 0)
      ((and (= arity 1) (eq? name 'add1)) 0)
      ((and (= arity 1) (eq? name 'sub1)) 0)
      ((and (= arity 1) (eq? name 'number?)) 0)
      (else 1))))

(define (count tree)
  (if (pair? tree)
      (if (eq? (car tree) 'cond)
          (check-cond tree) ; lol ur function goes here.
          (apply + (function-name? tree) (map count (cdr tree))))
      (bool-or-number? tree)))

(count '(add1 5 9))
(count '(atom? (sub1 6)))
(count '(eq? 6 test))