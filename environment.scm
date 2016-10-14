; We represent an environment as a list of frames

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; Each frame of an environment
; is represented as a list of pairs (variable value)

(define (make-frame variables values)
  (if (null? variables)
    '()
    (cons
      (cons
        (car variables)
        (car values))
      (make-frame
        (cdr variables)
        (cdr values)))))

(define (frame-bindings frame) frame)

(define (make-binding var val) (cons var val))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))

(define (first-binding frame) (car frame))
(define (rest-bindings frame) (cdr frame))


; TODO: clean up
(define (add-binding-to-frame! var val frame)
  (let ((f (cons (car frame) (cdr frame))))
    (set-car! frame (cons var val))
    (set-cdr! frame f)))


; Extend an environment by a new frame
; constructed from some new variables and values

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
        (error "Too many arguments supplied" vars vals)
        (error "Too few arguments supplied" vars vals))))

; Look up a variable in an environment,
; if we don't find the variable in the current frame,
; we search the enclosing environment

(define (scan bindings var null-fn found-fn)
  (cond ((null? bindings)
         (null-fn))
        ((eq? var (binding-variable (first-binding bindings)))
         (found-fn bindings))
        (else (scan (rest-bindings bindings) var null-fn found-fn))))

(define (env-loop env var function error-message)
  (if (eq? env the-empty-environment)
      (error error-message var)
      (let ((frame (first-frame env)))
        (scan (frame-bindings frame) var
              (lambda ()
                (env-loop
                  (enclosing-environment env) var function error-message))
              function))))

; Operations on environments

; Exercise 4.16, 1
(define (lookup-variable-value var env)
  (let ((result (env-loop env var
                          (lambda (bindings)
                            (binding-value (first-binding bindings)))
                          "Unbound variable")))
    (if (eq? result '*unassigned*)
      (error "Unassigned variable" var)
      result)))

; To set a variable to a new value
; in a specified environment,
; we scan for the variable,
; just as in lookup-variable-value
; and change the corresponding value when we find it

(define (set-variable-value! var val env)
  (env-loop env var
            (lambda (bindings)
              (set-car! bindings (make-binding var val)))
            "Unbound variable: SET!"))

; To define a variable,
; we search the first frame for a binding
; and change this if it exists.
; If no such binding exists,
; we adjoin one to the first frame

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan (frame-bindings frame) var
          (lambda () (add-binding-to-frame! var val frame))
          (lambda (bindings) (set-car! bindings (make-binding val var))))))

; Remove the binding of a given symbol from the environment,
; it is unclear if this should only remove the binding
; in the first frame of the environment
; or all bindings

; => Choice: only remove the binding from the first frame,
; so that it is possible to access an binding higher up in the environment
; an provide a second method to clear all bindings

(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (scan (frame-bindings frame) var
          (lambda () 'ok)
          (lambda (bindings)
            (let ((old-car (car bindings))
                  (old-cdr (cdr bindings)))
              (set-car! bindings (car old-cdr))
              (set-cdr! bindings (cdr old-cdr)))))))

;; Test suite
; (define test-env 
;   (extend-environment
;     '(a b c d)
;     '(1 2 3 4)
;     the-empty-environment))
; (display test-env)
; (newline)

; (define new-env (extend-environment '(e) '(5) test-env))
; (display new-env)
; (newline)

; (set! test-env new-env)

; (display (lookup-variable-value 'a test-env))
; (newline)
; (display (lookup-variable-value 'e test-env))
; (newline)

; (set-variable-value! 'a 10 test-env)
; (display (lookup-variable-value 'a test-env))
; (newline)

; (define-variable! 'f 100 test-env)
; (display (lookup-variable-value 'f test-env))
; (newline)

; (make-unbound! 'f test-env)
; (display (lookup-variable-value 'f test-env))
; (newline)

; Create an initial environment from primitive procedures
