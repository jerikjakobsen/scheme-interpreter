; CSc 335 Sections M and R
; Fall 2021

; December 21

; Final Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: JOHN JAKOBSEN
; TYPE YOUR NAME HERE!!!!!!!

; TYPE YOUR FULL EMAIL ADDRESS HERE: JJAKOBS001@citymail.cuny.edu
; TYPE YOUR FULL EMAIL ADDRESS HERE!!!!!!!!!!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; PLEASE MARK HERE WHETHER YOU TOOK THE ORAL EXAM

;;; yes, I took the oral, so this exam is worth 25 points  -------------

;;; no, I did not take the oral, so this exam is worth 35 points TRUE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; SAVE YOUR EXAM AS Lastname.Firstname.scm
; SAVE ANY ASSOCIATED WORK AS Lastname.Firstname.pdf

; QUESTIONS DURING THE EXAM: email me at dtroeger@ccny.cuny.edu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; INSTRUCTIONS

;; AFTER YOU RECEIVE THE EXAM

; read and sign and date the honesty pledge, below (REQUIRED!)

; start solving the exam problems as soon as you have signed and dated the honesty pledge

; NO LATER THAN 17:45, stop writing and SAVE YOUR EXAM AND THE ASSOCIATED PDF DOCUMENT (IF ANY)

; BEFORE 17:50 -- email your Lastname.Firstname.scm to me at dtroeger@ccny.cuny.edu


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; NOTE THAT I AM NOT ACCEPTING PDF FILES FOR THIS EXAM: JUST YOUR .SCM SOLUTION FILE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; LATE ARRIVING EXAMS WILL BE PENALIZED 15% FOR EVERY MINUTE LATE, STARTING AT 17:52. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone, with your ONLY reference
; the R5RS language manual.  SEARCHING FOR ANSWERS ON THE WEB IS CHEATING.  CONTRACTING WITH CHEGGS (ETC)
; TO SOLVE THE PROBLEM IS CHEATING.  WORKING WITH ANYONE ELSE IN PERSON OR VIA SOCIAL MEDIA IS CHEATING.
; CONSULTING NOTES OR A BOOK IS CHEATING. DO NOT CHEAT!!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; READ AND SIGN AND DATE:

; READ: I pledge my honor that the work submitted here is entirely my own: I have not cheated on this exam.

; SIGNATURE: John Jakobsen

; DATE: 12/21/2021

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Scoring Template FOR STUDENTS WHO TOOK THE ORAL - Do NOT Erase

;;;; Problem 1 Score and Percentage  (12 points possible)

;;;; Problem 2 Score and Percentage (13 points possible)

;;;; Total Score and Percentage (25 points possible)


;;;; Letter Grade


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Scoring Template FOR STUDENTS WHO DID NOT TAKE THE ORAL - Do NOT Erase

;;;; Problem 1 Score and Percentage  (12 points possible)

;;;; Problem 2 Score and Percentage (13 points possible)

;;;; Problem 3 Score and Percentage (10 points possible)

;;;; Total Score and Percentage (35 points possible)


;;;; Letter Grade

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed deeply so far in the context of the quizzes or homework:

; no vectors, no strings, no assignment ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR SOLUTIONS INTO THIS DOCUMENT, IMMEDIATELY FOLLOWING THE PROBLEM STATEMENTS.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Here are the exam problems.

; EVERYONE IS TO COMPLETE PROBLEMS 1 AND 2.

; PROBLEM 3 IS TO BE COMPLETED BY STUDENTS WHO DID NOT TAKE THE ORAL EXAM. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 1 (12 points - EVERYONE COMPLETE) Specify, design, develop, code, prove correct and test an R5RS Scheme program fpip?
; which inputs an s-expression s and which returns #t if s belongs to the class fpip, and #f otherwise, where
; fpip ("fully parenthesized infix + expressions") is defined

;  var ::=  a | b | c | d | e | f | g
;  fpip ::= var | (fpip + fpip)

;Preconditions: A proper s-expression is inputted.
;Postconditions: fpip? returns true if the provided s-expression belongs to the class fpip and false otherwise
;
;idea:
;Precheck on the data structure of fpip:
;var is represented by atoms of the same names (ie a, b ...)
;(fpip + fpip) is represented by a three element list with the center element the atom "+",
;and the first and last elements fpips.
;We use the inductive definition to design fpip?, we start by checking if the sexp is an atom in which case, we return true
;if the sexp is a var and false otherwise, if it is not an atom then since it is an s-expression the sexp must be a list, so
;we check the length of the list, if it is 3 then we can proceed, otherwise it must be malformed so we return false. If it was 3
;then we check if the second element of the list is "+", if it is we return the and of fpip? of the first and last elements of the list.
;
;Proof by structural induction
;
;Induction Hypothesis
;
;Let A(s) be the property that fpip? verfifies the properly formed s-expression s correctly, that is if s is a properly formed
;fpip it returns true and false otherwise.
;Assume A for sub-components of s.
;
;Base Case:
;If s is an atom we return true if it is one of the following: a, b, c, d, e, f, g.
;and false otherwise.
;
;Induction Step:
;
;Assuming fpip works for sub-components of s, then we can get the correct answer of fpip? s by
;checking if s is a three element list, if it is not return false, then checking if the center element is "+",
;if it is not return false, lastly by our induction hypothesis, AND the results of fpip? (car s) and fpip? (caddr s),
;this works since (car s) and (caddr s) are sub-components of s. This returns the correct answer because the first and last elements
;of s must be proper fpip expressions for s to be a proper fpip expression.
;
;Code

;Precondition: a is an atom
;Postcondition: if a is a var true is returned, otherwise false is returned

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (var? a)
  (cond ( (eq? a 'a) #t)
        ( (eq? a 'b) #t)
        ( (eq? a 'c) #t)
        ( (eq? a 'd) #t)
        ( (eq? a 'e) #t)
        ( (eq? a 'f) #t)
        ( (eq? a 'g) #t)
        (else #f)))

(define (fpip? s)
  (cond ( (atom? s) (var? s))
        ( (not (= 3 (length s))) #f)
        ( (not (eq? '+ (cadr s))) #f)
        ( else (and (fpip? (car s)) (fpip? (caddr s))))))

;Tests:

(define f1 '(a + b))
(define f2 '( (f + z) + (a + g) ))
(define f3 '( (f + f) + (a + (a + d)) ))
(define f4 '( (f + f) + (a + (z + d)) ))
(fpip? f1) ;True
(fpip? f2) ;False
(fpip? f3) ;True
(fpip? f4) ;False



; Problem 2 (13 points - EVERYONE COMPLETE) Add the special form if to TLS-Scheme by modifying the attached copy of TLS-scheme.
; Mark your additions clearly, and give a complete discussion, including enough detail on the proof of correctness of
; the augmented language (call it TLS-IF) to convince me that you know what you are doing, as well as working examples.

;Idea
;
;To add let to the tls interpreter we can add another type *if, which will be detected in the list-to-action,
;like all the other types, *if will be defined to take the expression to be evaluated as the first parameter and the table
;environment for the if blocks to be evaluated in. if will be of the form (if tls-expression1 tls-expression2 tls-expression3),
;where tls-expression1 should be true or false, if it is true tls-expression2 is returned, if it is false tls-expression3 is returned.
;This relies on the inductive definition of tls-expression, to find the value of tls-expression1, tls-expression2, tls-expression3 we use
;meaning on each of them.
;
;Proof of its correctness
;
;Induction Hypothesis:
;Assume that any occurrence of an if expression is evaluated correctly in any sub-expression of a correctly formed tls-expression.
;
;Base Case
;If an if expression appears in any of the primitves, (such as #t, #f, nummber, function names, etc) it is vacuously evaluated correctly,
;since the if statement cannot appear in such expressions. 
;
;
;Cases:
;
;Appears in cond:
;If if appears in cond it must appear in one of the sub-expressions, one of the question and answer pairs,
;since by our IH if is evaluated correctly in all sub-expressions of properly formed tls-expressions, if is evaluated correctly.
;
;Appears in lambda:
;If if appears in lambda the only place it can appear in is the body, and since the body is a sub-expression of a properly formed tls-expression
;if is evaluated correctly.
;
;Appears in application:
;An application must be one of two types, non-primitive and primitive, if it is a non-primitive then it is of the form
;( lambda-expression arg1 arg2 ...argn) where all args are properly formed tls-expressions, since it handles the lambda expression correctly (see above),
;the only thing we need to worry about is the arguments. Since the arguments are all sub-expressions of the non-primitive application, by our inductive hypothesis
;if is evaluated correctly in them. If it is a primitive then the only place if can appear is in the arguments of the primitive function,
;Since the arguments are all sub-expressions of the primitive application, by our inductive hypothesis
;if is evaluated correctly in them.
;
;Appears in identifier:
;The only way for if to appear in an identifier is for the identifier to be if, in this case the if expression should follow, which is evaluated correctly by
;*if, the second element is tested, if it is true the third element is returned otherwise the fourth element is returned.
;
;Appears in quote:
;The only place for if the appear in quote is in the argument of quote, it is evaluated correctly since it is not evaluated at all by
;tls-scheme interpreter.
;
;Test Cases Below

; Problem 3 (10 points - COMPLETE ONLY IF YOU DID NOT TAKE THE ORAL EXAM)
; Specify, design, develop, code and prove correct a complete R5RS program prime-factorization which inputs an integer
; n >= 1 and returns a list of pairs of the form (prime power) where prime occurs power times in the prime factorization of n.

; Thus

;;    (prime-factorization 1) = ()
;;    (prime-factorization 7) = ((7 1))
;;    (prime-factorization 30) = ((2 1) (3 1) (5 1))
;;    (prime-factorization 100) = ((2 2) (5 2))
;;    (prime-factorization 1024) = ((2 10))

; The output list must be sorted by primes, with smaller prime numbers appearing before larger prime numbers. Your solution
; MUST use the function filter.  It MUST also use accumulate and/or map.
(define (prime? n)
  (define (prime-tester divider)
    (cond ( (zero? (remainder n divider)) #f)
          ( (= divider (ceiling (sqrt n))) #t)
          ( else (prime-tester (+ 1 divider)))))
  (cond ( (= 2 n) #t)
        ( (= 3 n) #t)
        (else (prime-tester 2))))

(define (next-prime p)
  (if (prime? p) p (next-prime (+ 1 p))))

(define (least-prime-divider n)
  (define (iter p)
    (if (= (remainder n p) 0) p (iter (next-prime p))))
  (iter 2))
;Idea
;We can get the prime factorization of the integer n divided by the smallest prime that divides n with no remainder, then we add one to the power
;of the (smalles_prime_that_divides_n_with_no_remainder power),  then returns the prime factorization list with the updated prime power pair. If there is no
;prime power pair with the prime that was used to divide n then we add a new pair. This will force it to be in order of least to greatest pairs by prime, since
;we are always dividing n by the least prime. For this method we will use a helper function, least-prime-divider that returns the least prime that divides n with no
;remainder.
;
;Proof by Strong Induction
;
;Induction Hypothesis
;
;Assume the prime-factorization works for all k, 0 < k < n, where k is an integer.
;
;Base Case
;
;When n is one we can return the empty list.
;
;Inductive Step
;
;Let j be the least prime integer that divides n with no remainder, to get prime-factorization of n, all we need to do is get
;the prime-factorization of n/j, then add j to the list of prime power pairs (or update its count) that we get from
;(prime-factorization (/ n j)). This works by our strong inductive hypothesis.
; code
;(define (prime-factorization n)
;  (cond ( (= 1 n) `())
;        ( else (let ( (p (least-prime-divider n)))
;                 (map (prime-factorization (/ n p)) (lambda (x) (if (= car x)
;;;;

;least-prime-divider idea
; preconditions: n >= 2 and is an integer
; postcondition: the least prime divider that is an integer is returned
;We can find the least prime that divides a number n iteratively, we can have the starting prime, p, to start at, then n,
;if the p divides n with no remainder then we can return p, otherwise we can update p to be the closest prime that is greater
;than p. For this we will need a helper function next-prime, which returns the closest greater prime of the number inputted.
;
;GI
;
;p is prime and no prime less than p and greater than the initial starting prime, divides n
;
;Strong Enough?
;
;if p divides n with no remainder then by our stopping condition we can return p, and by the guess invariant p
;is prime and is the least prime that divides n.
;
;Weak Enough?
;We can start with p as 2, since 2 is the least prime, so there is no prime less than it.
;
;Preserved?
;
;Yes, if p does not divide n with no remainder then we can return least_prime_divider with the next-prime as p,
;which preserves the invariant because there exists no prime less than next_prime p that divides n.
;
;code

(define (least-prime-divider n)
  (define (iter p)
    (if (= (remainder n p) 0) p (iter (next-prime (+ 1 p)))))
  (iter 2))

;Tests

(least-prime-divider 10)
(least-prime-divider 21)
(least-prime-divider 49)

;next-prime idea
;We can iteratively find the next prime by adding one to prime until we reach a prime. next-prime accepts p, where
;p is a positive integer, it returns the closest prime greater than or equal to p. We stop and return p when p is prime.
;
;GI
;
;There is no prime between the initial p and p - 1.
;
;Strong Enough?
;Yes because there is no prime between the starting p + 1 and p-1, so if p is prime then it is
;the smallest prime after p, and therefore the closest prime integer after p.
;
;Weak Enough?
;Yes we can start with p as p+1.
;
;Preserved?
;Yes because on every call to next-prime we add 1 to p and since there are no primes of integers in between 1 and p + 1, the
;guess invariant is held.
;
;Code

(define (next-prime p)
  (if (prime? p) p (next-prime (+ 1 p))))
;
;prime? idea
;precon: n > 1
;postcond: if n is prime true is returned if n is not false is returned
;we divide n with every integer from 1 to (ceiling (sqrt n)), if n divides an integer in that range then we return false, otherwise
;we return true only when the divider = (ceiling (sqrt n)).
;
;Guess invariant
;
;There exists no integer >= divider that divides n with no remainder.
;
;Strong Enough?
;yes because when divider = (ceiling (sqrt n)),there is no number before sqrt n that divides n and since
;there is no number after sqrt n that divides n, n is prime.
;
;Weak Enough?
;yes we can start with divider at 2.
;
;Preserved?
;Yes because if divider divides n evenly then we can return false, otherwise we can add one to divider and call
;the function with divider + 1.
;
;code

(define (prime? n)
  (define (prime-tester divider)
    (cond ( (zero? (remainder n divider)) #f)
          ( (= divider (ceiling (sqrt n))) #t)
          ( else (prime-tester (+ 1 divider)))))
  (cond ( (= 2 n) #t)
        ( (= 3 n) #t)
        (else (prime-tester 2))))

(prime? 2)
(prime? 5)
(prime? 17)
(prime? 59)
(prime? 27)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; INSERT YOUR ANSWERS HERE.

; AGAIN: I AM NOT ACCEPTING PDF FILES FOR THIS EXAM.  Everything needs to be in your .scm file. 
























;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; HERE IS TLS-SCHEME.  THE COPY I INCLUDE HERE IS EXACTLY THE COPY THAT I POSTED EARLIER ON TEAMS.  YOU MAY INSERT
;;;; YOUR MODIFICATIONS FOR PROBLEM 2 DIRECTLY INTO THIS CODE- MARK CLEARLY THE CHANGES YOU HAVE MADE.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; CSc 335
; tls-scheme interpreter


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
         ( (eq? (car e) (quote if)) *if) ;;;;;;------------------CHANGED------------------;;;;;;
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

(define *if (lambda (e table) ;;;;;;------------------CHANGED------------------;;;;;;
               (if (meaning (bool-statement e) table) (meaning (true-statement e) table) (meaning (false-statement e) table)))) 
(define bool-statement second)
(define true-statement third)
(define false-statement (lambda (lst)
                          (cadddr lst)))

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

(value '(if (zero? 1) 5 3))
(value '(cond ((eq? (quote t) (quote e)) 1)
              (else (if (null? (quote ())) 2 3))))
(value '( (lambda (x) (cond ((eq? (quote t) (quote e)) 1)
              (else (if (zero? x) 2 3)))) 1))
(value '( (lambda (x) (cond ((eq? (quote t) (quote e)) 1)
              (else (if (zero? x) 2 3)))) 0))
(value '(quote (if #t #f #t)))
(value '((lambda (x) (add1 (if x 1 0))) #t) )
(value '((lambda (x) (add1 (if x 1 0))) #f) )