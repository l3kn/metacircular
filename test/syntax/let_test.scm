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

(define input 
  '(let* ((a 1)) (+ a 1)))
(define output
  '(let ((a 1)) (+ a 1)))
(assert-equal 
  "let*->nested-lets with a single binding"
  output
  (let*->nested-lets input))

(define input 
  '(let* ((a 1) (b (+ a 1))) (+ a b)))
(define output
  '(let ((a 1))
     (let ((b (+ a 1)))
       (+ a b))))
(assert-equal 
  "let*->nested-lets with a single expression in the body"
  output
  (let*->nested-lets input))

(define input 
  '(let* ((a 1) (b (+ a 1))) (+ a b) (* a b)))
(define output
  '(let ((a 1))
     (let ((b (+ a 1)))
       (+ a b)
       (* a b))))
(assert-equal 
  "let*->nested-lets with multiple expressions in the body"
  output
  (let*->nested-lets input))
