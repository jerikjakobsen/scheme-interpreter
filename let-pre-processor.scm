;pre processor for let

(define (first lst) (car lst))
(define (second lst) (cadr lst))
(define (third lst) (caddr lst))
(define (build3 x1 x2 x3)
  (cons x1 (cons x2 (cons x3 '()))))
(define (build x1 x2)
  (cons x1 (cons x2 '())))
(define (else? x)
  (eq? 'else x))
(define (atom? x)
  (and (not (pair? x)) (not (list? x))))

(define (expression-to-action-replacer exp)
  (cond ( (atom? exp) const-replacer)
        ( else (list-to-action-replacer exp))))

(define (list-to-action-replacer exp)
  (cond ( (eq? 'cond (car exp)) cond-replacer)
        ( (eq? 'let (car exp)) let-replacer)
        ( (eq? 'quote (car exp)) const-replacer)
        ( (list? (car exp)) non-primitive-replacer)
        ( else primitive-replacer)))

(define (cond-replacer exp)
  (cons 'cond (map (lambda (line)
                   (cond ( (else? (first line)) (build 'else (pre-process (second line))))
                         ( else (build (pre-process (first line)) (pre-process (second line)))))) (cdr exp))))

(define (let-replacer exp)
  (cons (build3 'lambda (let-formals-to-names (second exp)) (pre-process (third exp)))
        (let-formals-to-values (second exp))))

(define (let-formals-to-names formals)
  (map (lambda (x) (first x)) formals))

(define (let-formals-to-values formals)
  (map (lambda (x) (pre-process (second x))) formals))

(define (const-replacer exp) exp)

(define (non-primitive-replacer exp)
  (cons (build3 'lambda (second (first exp)) (pre-process (third (first exp))))
        (map pre-process (cdr exp))))

(define (primitive-replacer exp)
  (cons (first exp) (map pre-process (cdr exp))))

(define (pre-process exp)
  ((expression-to-action-replacer exp) exp))

;Tests

(pre-process '(let ( (x 3) (y 4)) (cond (else (add1 x)))))
(pre-process '(cond ( (zero? (add1 (let ((x -1)) (add1 x)))) ((lambda (x) (let ((y x)) (add1 y))) 5))))
(pre-process '(quote x)) ;;; Check this over, not sure why it returns 'x
(define (add1 x) (+ 1 x))

;(let ( (x 3) (y 4)) (cond (else (add1 x))))
;(cond ((zero? (add1 ((lambda (x) (add1 x)) -2))) ((lambda (x) ((lambda (y) (add1 y)) x)) 5)))