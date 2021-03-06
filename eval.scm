; Environment model of evaluation:
;  1. To evaluate a combination
;     (a compound expression other than a special form),
;     evaluate the subexpressions
;     and then apply the value of the operator subexpression
;     to the values of the operand subexpressions
;
;  2. To apply a compound procedure
;     to a set of arguments,
;     evaluate the body of the procedure
;     in a new environment.
;     To construct this environment,
;     extend the environment part of the procedure object
;     by a frame in which the formal parameters of the procedure
;     are bound to the arguments
;     to which the procedure is applied

(use srfi-1)
(include "environment.scm")
(include "analyze.scm")
(include "primitives.scm")
(include "syntax/let.scm")
(include "syntax/cond.scm")
(include "thunk.scm")

(define (lazy-pair? exp)
  (tagged-list? exp 'lazy-pair))

(define (eval_ exp env)
  ; (print "eval " exp)
  (cond ((self-evaluating? exp)
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((quoted? exp)
         (text-of-quotation exp env))
        ((tagged-list? exp 'cons)
         (list 'lazy-pair
               (delay-it-memo (cadr exp) env)
               (delay-it-memo (caddr exp) env)))
        ((tagged-list? exp 'car)
         (cadr (actual-value (cadr exp) env)))
        ((tagged-list? exp 'cdr)
         (caddr (actual-value (cadr exp) env)))
        ((assignment? exp)
         (eval-assignment exp env))
        ((definition? exp)
         (eval-definition exp env))
        ((if? exp)
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure
           (lambda-parameters exp)
           (lambda-body exp)
           env))
        ((begin? exp)
         (eval-sequence
           (begin-actions exp)
           env))
        ((cond? exp)
         (eval_ (cond->if exp) env))
        ((let? exp)
         (eval_ (let->lambda exp) env))
        ((let*? exp)
         (eval_ (let*->nested-lets exp) env))
        ((letrec? exp)
         (eval_ (letrec->let-and-set exp) env))
        ((application? exp)
         ; (print "application? " exp)
         (apply_ (actual-value (operator exp) env)
                 (operands exp)
                 env))
        (else
          (error "Unknown expression type: EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval_ exp env)))

(define (apply_ procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-manipulated-args
               arguments
               env
               (procedure-parameter-tags procedure))
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-arg-values exps env)
  (map (lambda (exp) (actual-value exp env)) exps))

(define (list-of-manipulated-args exps env tags)
  ; (print "args: " tags)
  (map (lambda (exp tag)
         (cond ((eq? tag 'lazy)
                (delay-it exp env))
               ((eq? tag 'lazy-memo)
                (delay-it-memo exp env))
               ((eq? tag 'normal)
                (actual-value exp env))
               (else
                 (error "Unknown tag:
                         LIST-OF-MANIPULATED-ARGS" tag))))
       exps tags))

; Evaluate the predicate of an if expression,
; depending on the result, evaluate the consequent or the alternative
;
; true? highlights the issue of the connection
; between an implemented language
; and an implementation language

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
    (eval_ (if-consequent exp) env)
    (eval_ (if-alternative exp) env)))

; Evaluate a sequence of expressions
; in the order in which they occur

(define (eval-sequence exps env)
  ; (print "eval-sequence " exps)
  (if (last-exp? exps)
      (eval_ (first-exp exps) env)
      (begin
        (eval_ (first-exp exps) env)
        (eval-sequence (rest-exps exps) env))))

; Call eval to find the value to be assigned
; and install it in the designated environment

(define (eval-assignment exp env)
  (set-variable-value!
    (assignment-variable exp)
    (eval_ (assignment-value exp) env)
    env)
  'ok)

; Definitions are handled in a similar manner

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval_ (definition-value exp) env)
    env)
  'ok)

;
; Here is the specification of the syntax of our language
;

; The only self evaluating items are numbers and strings

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

; Variables are represented by symbols

(define (variable? exp) (symbol? exp))

; Quotations have the form (quote <text-of-quotation>)
; 'a would be seen by the evaluator as (quote a)

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp env)
  (let ((body (cadr exp)))
    (if (list? body)
      (if (null? body)
        '()
        (eval_ (quoted-list->nested-cons body) env))
      (cadr exp))))

(define (quoted-list->nested-cons qlist)
  (if (null? qlist)
    ; We need to double-quote here, because the results are passed through eval_ again
    ''()
    (list 'cons (car qlist) (quoted-list->nested-cons (cdr qlist)))))

; Identify lists beginning with a designated symbol

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

; Assignments have the form (set! <var> <value>)

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

; Definitions can have two forms:
;   (define <var> <value>)
; or 
;   (define (<var> <param_1> ... <param_n>)
;     <body>)
;
; The latter form is syntactic sugar for
;
; (define <var>
;   (lambda (<param_1> ... <param_n>)
;      <body>))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)    ; First form
      (caadr exp))) ; Second form

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda
        (cdadr exp)   ; parameters
        (cddr exp)))) ; body

; Lambda expressions are lists that begin with the symbol lambda

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

; We also provide a constructor for lambda expressions,
; which is used by definition-value, above

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; Conditionals begin with 'if
; and have a predicate,
; a consequent,
; and an (optional) alternative.
; If the expression has no alternative part,
; we provide false as the alternative

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

; We also provide a constructor for if expressions,
; to be used by cond->if

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin packages a sequence of expressions into a single expression.

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; We also include a constructor sequence->exp
; (for use by cond->if)
; that transforms a sequence into a single expression,
; using begin if necessary

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

; A procedure application is any compound expression
; that is not one of the above expression types

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; Testing of predicates

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

; Representing procedures
; 
; We assume that we have available the following procedures:
;  * (apply-primitive-procedure <proc> <args>)
;  * (primitive-procedure? <proc>)

; Exercise 4.32

(define (argument-tag arg)
  (if (pair? arg)
      (cadr arg)
      'normal))

(define (remove-argument-tag arg)
  (if (pair? arg)
      (car arg)
      arg))

(define (make-procedure parameters body env)
  (list 'procedure
        (map remove-argument-tag parameters)
        body
        env
        (map argument-tag parameters)))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (procedure-parameter-tags p) (car (cddddr p)))

(define (setup-environment)
  (let ((initial-env
          (extend-environment
            (primitive-procedure-names)
            (primitive-procedure-objects)
            the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (eval-file file env)
  (if (not (null? file))
    (begin
      (eval_ (car file) env)
      (eval-file (cdr file) env))))

