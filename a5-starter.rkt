#lang racket
(require plai/datatype)
(require rackunit)
(require "exn-311.rkt")
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

;; CPSC311 Winter Term 1
;; Assignment 5: D'oh! I did it again...

;; Released: Monday, November  9, 2026
;; Due:      Friday, November 20, 2026 10pm (late deadline Nov. 22 10pm)

;; Student 1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name: 
;; Student Number: 
;; CWL: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Student 2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name: 
;; Student Number: 
;; CWL: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PART 1: For what is a macro? (My WAE)
;;

;; (sleep s) pauses execution for s seconds.
;; Try running the following in your interaction window:
#;#;#;
"yawwn, so tired..."

(sleep 2)

"so well rested!"

;; We can use sleep to help us demonstrate that if does not evaluate all of its
;; arguments -- it is strict only in its first position

;; This doesn't wait, ...
#;
(if #t
    7
    (begin 
      (sleep 2)
      "false branch #1"))

;; ... but this does
#;
(if #f
    7
    (begin 
      (sleep 2)
      "false branch #2"))

;; Let's abstract this sleeping capability.

;; Natural X -> X
;; sleep for s seconds, then produce value v
(define (sleep-then-v s v)
  (begin    
    (sleep s)
    v))

;; This doesn't quite work :(

#;
(sleep-then-v 3 (begin (display "starting\n")
                       (sleep 2)
                       (display "stopping\n")
                       9))

;; Okay, we need to delay evaluation. Well, we could require the user to provide
;; a "thunk" (zero-argument function)

;; Natural ( -> X ) -> X
;; sleep for s seconds, then produce value v
(define (sleep-then-thaw s thunk)
  (begin    
    (sleep s)
    (thunk)))

#;
(sleep-then-thaw 3
                 (lambda () (begin (display "starting th\n")
                                   (sleep 2)
                                   (display "stopping th\n")
                                   9)))

;; But it's annoying to have to write a thunk :/
;; Is there another way? YES!

;; Instead of using procedural abstraction, we use *syntactic abstraction*
;; in the form of a Racket macro.
(define-syntax sleep-then
  (syntax-rules ()
    [(_ s e) (begin (sleep s) e)]))
#;
(sleep-then 3 (begin (display "starting\n")
                     (sleep 2)
                     (display "stopping\n")
                     9))

;; The Racket *compiler* rewrites (sleep-then s e) into (begin (sleep s) e)
;; during *macro expansion*, which completely precedes execution.

;; PROBLEM 1:
;; The languages we have implemented support a "with" form
;; {with {x e1} e2}

;; which can be desugared to
;; {{fun {x} e2} e1}

;; Let's add it to Racket using macros, but also make it multi-identifier

(define-syntax my-with
  (syntax-rules ()
    [(_ ([x* e*] ...) e)
     ((lambda (x* ...) e) e* ...)]))

;; The Racket expression
#;
(let* ([a  6]
       [a (+ a 1)]
       [a (+ a 3)])
  a)

;; is equivalent to
#;
(let ([a 6])
  (let ([a (+ a 1)])
    (let ([a (+ a 3)])
      a)))

;; Introduce a macro called my-with*, implemented in terms of my-with, that does
;; the same thing. (Uncomment the definition and the tests as you design it!)
;; Type: (my-with* ([Symbol Any] ...) Any2) -> Any2
#;(define-syntax my-with*
    (syntax-rules ()
      (...))) ;; stub

;; It *is* actually possible to directly test the syntax manipulations;
;; however, we will (as usual) test behaviour instead.
;(check-equal? (my-with* () 9) 9)
;(check-equal? (my-with* ([x 8]) 9) 9)
;(check-equal? (my-with* ([x 8]) x) 8)
;(check-equal? (my-with* ([x 8] [y 9]) (+ x y)) 17)
;(check-equal? (my-with* ([x 8] [y (+ x 1)]) (+ x y)) 17)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments

;; Env is Symbol -> Value
;; (For definition of Value, see below in PART 2.)


;; Env
;; the empty environment
(define empty-env
  (lambda (x)
    (error/311 'empty-env "unbound identifier ~a" x)))


;; Env Symbol Value -> Env
;; extend env with a binding from x0 to v
(define (extend-env env x0 v)
  (lambda (x)
    (if (symbol=? x x0)
        v
        (env x))))


;; Env Symbol -> Value
;; look up the identifier x in env
;; Effect: Signal an error if variable is not present
(define (lookup-env env x) (env x))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2: Imagine all the values

;; NDFWAE (Non-deterministic FWAE) is yet another FWAE-based language that
;; has first-class (higher-order) functions and the if0 operator,
;; as well as two extra features:
;; - choice: represents a non-deterministic choice between two NDFWAEs.
;;           "non-deterministic" here means that each choice is possible
;;           (i.e., if you want to imagine it this way, the "true" value
;;           of the program could be either choice branch)
;; - ignore: used when we want to disregard one of the choices made

;; The choice operator allows us to model external choices in our programs, 
;; such as: a program that flips a coin
#;'(with 'coinflip (choice (num 0) (num 1))
         (if0 (id 'coinflip)
              (num 0)
              (num 1)))
;; a program that rolls a die
#;'(with 'dice-roll
         (choice (num 1)
                 (choice (num 2)
                         (choice (num 3)
                                 (choice (num 4)
                                         (choice (num 5) (num 6))))))
         (id 'dice-roll))
;; We can even model conditional distributions! Here's a program
;; that produces the results of the coinflip, *given* that it is tails:
#;'(with 'coinflip (choice (num 0) (num 1)) ;; let 0 be heads; 1 be tails
         (if0 (id 'coinflip)
              (ignore)
              (id 'coinflip)))
;; and one that produces the results of a second coinflip, given the
;; first coinflip is tails:
#;'(with 'coinflip (choice (num 0) (num 1)) ;; let 0 be heads; 1 be tails
         (if0 (id 'coinflip)
              (ignore)
              (with 'coinflip (choice (num 0) (num 1))
                    (id 'coinflip))))

(define-type Value
  [numV (n number?)]
  [funV (x symbol?) (named NDFWAE?) (env procedure?)])
;; interp. Represents a value of the FWAE language.

#;
(define (fn-for-value v)
  (type-case Value v
    [numV (n) (... n)]
    [funV (x named env) (... x (fn-for-ndfwae name) env)]))

(define-type NDFWAE    
  [num (n number?)] 
  [add (lhs NDFWAE?) (rhs NDFWAE?)]
  [sub (lhs NDFWAE?) (rhs NDFWAE?)]
  [with (name symbol?) (named-expr NDFWAE?) (body NDFWAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body NDFWAE?)]
  [app (rator NDFWAE?) (arg NDFWAE?)]
  [if0 (predicate NDFWAE?) (consequent NDFWAE?) (alternative NDFWAE?)]
  [choice (lhs NDFWAE?) (rhs NDFWAE?)]
  [ignore])
;; interp. Expressions in a language that supports applying first-class
;; functions. Its syntax is defined by the following BNF:
;; <NDFWAE> ::= <num>
;;            | {+ <NDFWAE> <NDFWAE>}
;;            | {- <NDFWAE> <NDFWAE>}
;;            | {with {<id> <NDFWAE>} <NDFWAE>}
;;            | <id>
;;            | {<NDFWAE> <NDFWAE>}
;;            | {if0 <NDFWAE> <NDFWAE> <NDFWAE>}
;;            | {fun {<id>} <NDFWAE>}
;;            | {choice <NDFWAE> <NDFWAE>}
;;            | ignore

(define AE1 (num 1))
(define AE2 (num 2))

(define NDFWAE1 (with 'coinflip (choice (num 0) (num 1))
                      (if0 (id 'coinflip)
                           (num 100)
                           (num 0))))
(define NDFWAE2 (with 'coinflip (choice (num 0) (num 1))
                      (if0 (id 'coinflip)
                           (ignore)
                           (choice (num 0) (num 1)))))

#;
(define (fn-for-ndfwae ndfwae)
  (type-case NDFWAE ndfwae
    [num (n) (... n)]
    [add (l r) (... (fn-for-ndfwae l)
                    (fn-for-ndfwae r))]
    [sub (l r) (... (fn-for-ndfwae l)
                    (fn-for-ndfwae r))]
    [with (x named body)
          (... x
               (fn-for-ndfwae named)
               (fn-for-ndfwae body))]
    [id (x) (... x)]
    [fun (x body)
         (... x (fn-for-ndfwae body))]
    [app (rator rand)
         (... (fn-for-ndfwae rator)
              (fn-for-ndfwae rand))]
    [if0 (p c a)
         (... (fn-for-ndfwae p)
              (fn-for-ndfwae c)
              (fn-for-ndfwae a))]
    [choice (lhs rhs)
            (... (fn-for-ndfwae lhs)
                 (fn-for-ndfwae rhs))]
    [ignore () (...)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Interface
;;

;; (fail/eff) - fail the current computation
;; (choice/eff c c* ...) - concatenate the given computations

;; The Generic Interface Components:
;; (return/eff e) - return all possible values of e
;; (run/eff c) - run an effectful computation, producing all possible results
;; (let/eff ([x c1] c2)) - bind x to each successful value of c1 in c2
;; (let/eff* ([x* c1*] ...) c2) - sequentialize instances of let/eff

;;
;; Effect Implementation
;;

;; Computation is (listof Value)
;; interp. A computation that yields all of its possible values

;; -> Computation
;; the empty computation
(define (fail/eff) empty)


;; (Computation ...) -> Computation
(define choice/eff append)


;; Value -> Computation
;; "return" a successful value (by embedding it in a Computation)
(define (return/eff v)
  (list v))


;; Computation -> (listof Value)
;; "run" the computation to yield all possible values
(define (run/eff c) c)


;; (X -> (listof Y)) (listof X) -> (listof Y)
;; Apply fn to each value in lox, concatenating the resulting
;; lists of Ys into a single list
(define (flat-map fn lox)
  (foldr (lambda (v acc) (append (fn v) acc)) empty lox))

;; PROBLEM 2: Complete the implementation of let/eff
;; HINT: flat-map might be helpful
;; Compose two computations
(define-syntax let/eff
  (syntax-rules ()
    [(_ ([x c1]) c2)
     #f]))


;; Compose many computations
(define-syntax let/eff*
  (syntax-rules ()
    [(_ () c) c]
    [(_ ([x c1] [x* c1*] ...) c2)
     (let/eff ([x c1])
              (let/eff* ([x* c1*] ...) c2))]))

;;
;; End of Effect Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PROBLEM 3:
;; We want to write an interpreter that returns a list of all possible outcomes
;; Below, you'll find a partially implemented interp-ndfwae. Your goal is to
;; complete it by using the effect abstraction we defined above.
;;
;; Do NOT violate encapsulation by directly manipulating Computations!
;; Instead, use only the operations defined in the effect abstraction interface.

;; Value -> Number
;; extract the number from v
;; EFFECT: Signal an error if v does not represent a numV
(define (value->num v)
  (type-case Value v
    [numV (n) n]
    [funV (x body env)
          (error/311
           'value->num "Expected a number, got function: ~a" v)]))


;; Value (Symbol NDFWAE Env -> X) -> X
;; extract the fields of v and apply fn to them
;; EFFECT: Signal an error if v does not represent a funV
(define (value->apply-fun v fn)
  (type-case Value v
    [funV (x body env) (fn x body env)]
    [numV (n) (error/311
               'value->apply-fun "Expected a function, got ~a" v)]))


;; NDFWAE -> (listof Value)
;; Interprets an NDFWAE program and yields all possible outcomes
;; EFFECT: Signal an error if any possible program has a runtime error
(define (interp-ndfwae ndfwae)
  (local [;; Value Value -> Computation
          (define (add-values l r)
            (return/eff (numV (+ (value->num l) (value->num r)))))

          ;; Value Value -> Computation
          (define (sub-values l r)
            (return/eff (numV (- (value->num l) (value->num r)))))

          ;; Value Value -> Computation
          (define (apply-value rator rand)
            (value->apply-fun rator
                              (lambda (x body env)
                                (recur body (extend-env env x rand)))))

          ;; NDFWAE Env -> Computation
          (define (recur f env)
            (type-case NDFWAE f
              [num (n) (return/eff (numV n))]
              [add (l r) (... (recur l env)
                              (recur r env))]
              [sub (l r) (... (recur l env)
                              (recur r env))]
              [with (x named body)
                    (... (recur named env)
                         (recur body env))]
              [id (x) (return/eff (lookup-env env x))]
              [fun (x body) (... x body env)]
              [app (rator rand)
                   (... (recur rator env)
                        (recur rand env))]
              [if0 (p c a) (... (recur p env)
                                (recur c env)
                                (recur a env))]
              [choice (lhs rhs) (... (recur lhs env)
                                     (recur rhs env))]
              [ignore () (...)]))]
    (run/eff (recur ndfwae empty-env))))


(check-equal? (interp-ndfwae AE1) (list (numV 1)))
(check-equal? (interp-ndfwae AE2) (list (numV 2)))
(check-equal? (interp-ndfwae NDFWAE1) (list (numV 100) (numV 0)))
(check-equal? (interp-ndfwae NDFWAE2) (list (numV 0) (numV 1)))

;; add more tests!

