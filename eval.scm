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
(include "scan_out_defines.scm")

(define (eval_ exp env) ((analyze exp) env))

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

(define (text-of-quotation exp)
  (cadr exp))

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

(define (make-procedure parameters body env)
  ; (list 'procedure parameters (scan-out-defines body) env))
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (setup-environment)
  (let ((initial-env
          (extend-environment
            (primitive-procedure-names)
            (primitive-procedure-objects)
            the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

