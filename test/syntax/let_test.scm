(include "test/test_helper.scm")
(include "syntax/let.scm")
(include "eval")

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

(define input 
  '(let () (+ 1 2)))
(define output
  '(+ 1 2))
(assert-equal 
  "let with a no bindings"
  output
  (let->lambda input))

(define input 
  '(let () (+ 1 2) (* 2 4)))
(define output
  '(begin (+ 1 2) (* 2 4)))
(assert-equal 
  "let with a no bindings but multiple expressions in the body"
  output
  (let->lambda input))
