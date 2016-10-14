(include "syntax/let.scm")

(define input 
  '(let ((u 1) (v 2)) (+ u v)))
(define output
  '((lambda (u v) (+ u v)) 1 2))

(define (assert-equal name expected result)
  (if (equal? expected result)
      (begin
        (display "Test \"")
        (display name)
        (display "\" passed\n"))
      (begin
        (display "Test \"")
        (display name)
        (display "\" failed: \n")
        (display "  expected ")
        (display expected)
        (newline)
        (display "  got      ")
        (display result))))

(define input 
  '(let ((u 1) (v 2)) (+ u v)))
(define output
  '((lambda (u v) (+ u v)) 1 2))
(assert-equal 
  "let with a single expression in the body"
  output
  (let->lambda input))

(define input 
  '(let ((u 1) (v 2)) (+ u v) (- u v)))
(define output
  '((lambda (u v) (+ u v) (- u v)) 1 2))
(assert-equal 
  "let with multiple expressions in the body"
  output
  (let->lambda input))
