(define (analyze exp)
  ; (print "analyzing " exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp)
         (analyze-quoted exp))
        ((variable? exp)
         (analyze-variable exp))
        ((assignment? exp)
         (analyze-assignment exp))
        ((definition? exp)
         (analyze-definition exp))
        ((if? exp)
         (analyze-if exp))
        ((lambda? exp)
         (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence
           (begin-actions exp)))
        ((cond? exp)
         (analyze (cond->if exp)))
        ((let? exp)
         (analyze (let->lambda exp)))
        ((let*? exp)
         (analyze (let*->nested-lets exp)))
        ((letrec? exp)
         (analyze (letrec->let-and-set exp)))
        ((application? exp)
         (analyze-application exp))
        (else
          (error "UNknown expression type: ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (lambda (env) (text-of-quotation exp)))

(define (analyze-variable exp)
  (lambda (env)
    (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (val (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (val env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (val (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (val env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pred (analyze (if-predicate exp)))
        (con (analyze (if-consequent exp)))
        (alt (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (actual-value (pred env)))
          (con env)
          (alt env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (body (analyze-sequence (scan-out-defines (lambda-body exp)))))
    (lambda (env)
      (make-procedure vars body env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE")
        (loop (car procs) (cdr procs)))))

(define (analyze-application exp)
  (let ((function (analyze (operator exp)))
        (arguments (map analyze (operands exp))))
    (lambda (env)
      (execute-application
        (function env)
        (map (lambda (argument) (delay-it argument env))
             arguments)))))

(define (actual-value exp)
  (force-it exp))

(define (delay-it exp env)
  (cond ((self-evaluating? exp) exp)
        ((thunk? exp) thunk)
        (else (list 'thunk exp env))))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result
                 (actual-value
                   ((thunk-exp obj) (thunk-env obj)))))
           (set-car! obj 'evaluated-thunk)
           ; replace exp with its value
           (set-car! (cdr obj) result)
           ; forget unneeded env
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (thunk? exp)
  (tagged-list? exp 'thunk))
(define (thunk-exp exp)
  (cadr exp))
(define (thunk-env exp)
  (caddr exp))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc (map actual-value args)))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
            (procedure-parameters proc)
            args
            (procedure-environment proc))))
         (else (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))


