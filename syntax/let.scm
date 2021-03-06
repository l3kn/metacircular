(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))
(define (letrec? exp) (tagged-list? exp 'letrec))

; Ex. 4.6

(define (let->lambda exp)
  (if (null? (let-bindings exp))
    (sequence->exp (let-body exp))
    (expand-let (let-bindings exp) (let-body exp))))

(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-binding-variable binding) (car binding))
(define (let-binding-value binding) (cadr binding))

(define (make-let-binding variable value)
  (list variable value))

(define (split-bindings bindings)
  (cons
    (map let-binding-variable bindings)
    (map let-binding-value bindings)))

(define (expand-let bindings body)
  (let ((vars (car (split-bindings bindings)))
        (exps (cdr (split-bindings bindings))))
    (cons (cons 'lambda (cons vars body)) exps)))

; Ex. 4.7

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (let*->nested-lets exp)
  (if (null? (let-bindings exp))
    (sequence->exp (let-body exp))
    (expand-nested-let (let-bindings exp) (let-body exp))))

(define (expand-nested-let bindings body)
  (if (null? (cdr bindings)) ; last binding
    (make-let bindings body)
    (make-let (list (car bindings))
              (list (expand-nested-let (cdr bindings) body)))))

; Ex 4.20

(define (letrec->let-and-set exp)
  (if (null? (let-bindings exp))
    (sequence->exp (let-body exp))
    (expand-letrec (let-bindings exp) (let-body exp))))

(define (binding->set! binding)
  (cons 'set! (cons (binding-variable binding) (binding-value binding))))

(define (binding->unassigned binding)
  (make-let-binding (binding-variable binding) (quote '*unassigned*)))

(define (expand-letrec bindings body)
  (make-let
    (map binding->unassigned bindings)
    (append
      (map binding->set! bindings)
      body)))
