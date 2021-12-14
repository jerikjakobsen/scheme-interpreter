(define (make-apair name value) (cons name value))
(define (aname apair) (car apair))
(define (aval apair) (cdr apair))
(define (initial-table formal) (car '()))
(define (extend-entry apair entry)
  (cons apair entry))
(define (extend-table entry table)
  (cons entry table))
(define empty-table '())

(define (build-entry names values)
  (cond ( (null? names) names)
        ( else (extend-entry (make-apair (car names) (car values)) (build-entry (cdr names) (cdr values))))))

(define (lookup-in-table name table lookup-f)
  (cond ( (null? table) (lookup-f name))
        ( else (lookup-in-entry name (car table)
                               (lambda (name) (lookup-in-table name (cdr table) lookup-f))
                               ))))

(define (lookup-in-entry name entry lookup-f)
  (cond ( (null? entry) (lookup-f name))
        ( (eq? name (aname (car entry))) (aval (car entry)))
        ( else (lookup-in-entry name (cdr entry) lookup-f))))

(define entry1 (build-entry '(a b c d) '(1 2 3 4)))
(define entry2 (build-entry '(z x y d) '(6 7 8 2)))

(define table (extend-entry entry2 (extend-entry entry1 empty-table)))

(lookup-in-table 'c table initial-table)

        
(define (value e)
  (meaning e '() ))

(define (meaning e table)
  ( (expression-to-action e) e table))

(define (atom? a)
  (AND (Not (pair? a)) (not (null? a))))

(define (expression-to-action e)
  (cond ( (atom? e) (atom-to-action e) )
        ( else (list-to-action e))))

(define (atom-to-action e)
  (cond ( (number? e) *const)
        ( (eq? e #t) *const)
        ( (eq? e #f) *const)
        ( (eq? e 'car) *const)
        ( (eq? e 'cons) *const)
        ( (eq? e 'cdr) *const)
        ( (eq? e 'null?) *const)
        ( (eq? e 'zero?) *const)
        ( (eq? e 'add1) *const)
        ( (eq? e 'sub1) *const)
        ( else *identifier)))

(define *identifier (lambda (e table)
(lookup-in-table e table initial-table)))

  

(define (list-to-action e)
  (cond ( (atom? (car e))
          (cond ( (eq? (car e) 'quote) *quote)
                ( (eq? (car e) 'lambda) *lambda)
                ( (eq? (car e) 'cond) *cond)
                (else *application)
                ))
        (else *application)))

(define (*const e table)
  (cond ( (number? e) e)
        ( (eq? e #t) e)
        ( (eq? e #f) e)
        ( else (list 'primitive e))))

(define (*lambda e table)
  (list 'non-primitive (list table (cdr e))))

(define (*quote e table)
  (cadr e))
  
(define (*cond e table)
  (apply-cond (cdr e) table))

(define (apply-cond cond-lines table)
  (cond ( (eq? (car-predicate cond-lines) 'else) (meaning (car (cdr (car cond-lines))) table))
        ( (meaning (car-predicate cond-lines) table) (meaning (car-answer cond-lines) table))
        ( else (apply-cond (cdr cond-lines) table))))
                         
(define (car-predicate cond-line)
  (car (car cond-line)))
(define (car-answer cond-line)
  (car (cdr (car cond-line))))

(define (*application e table)
  (apply-tls (meaning (car e) table)
         (evlis (cdr e) table)))

(define (evlis lst table)
  (map (lambda (arg) (meaning arg table)) lst))

(define (apply-tls func args)
  (cond ( (primitive? func) (apply-primitive (cadr func) args))
        ( (non-primitive? func) (apply-closure (cdr func) args))
        ))

(define (primitive? func)
  (eq? (car func) 'primitive))

(define (non-primitive? func)
  (eq? (car func) 'non-primitive))

(define (apply-primitive name args)
  (cond ( (eq? name 'car) (car (car args)))
        ( (eq? name 'cons) (cons (car args) (cadr args)))
        ( (eq? name 'cdr) (cdr (car args)))
        ( (eq? name 'null?) (null? (car args)))
        ( (eq? name 'zero?) (zero? (car args)))
        ( (eq? name 'add1) (+ 1 (car args)))
        ( (eq? name 'sub1) (- (car args) 1))))

(define (apply-closure closure args)
  (meaning (body-of closure) (add-entry (make-entry (formals-of closure) args) (table-of closure))))

(define body-of (lambda (e) (car (cdr (cdr e)))))
(define table-of (lambda (e) (car e)))
(define formals-of (lambda (e) (car (cdr e))))