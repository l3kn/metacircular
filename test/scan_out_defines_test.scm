(include "test/test_helper.scm")
(include "eval.scm")

(define input
  '(lambda (var1 var2)
     (define u (e1))
     (define v (e2))
     (e3)))
(define output
  '((let ((u '*unassigned*)
          (v '*unassigned*))
     (set! u (e1))
     (set! v (e2))
     (e3))))

(assert-equal
  "Example from the SICP book"
  output
  (scan-out-defines (lambda-body input)))

(define input
  '((lambda (a) (define u 2) (define v 3) (+ a u v)) 1)
)

(assert-equal
  "Evaluation of a lambda"
  6
  (eval_ input (setup-environment)))


