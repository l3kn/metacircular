# metacircular

> A scheme evaluator (in no way compliant to any specification)
> with some strange features (e.g. optional lazy evaluation for arguments)

```scheme
(define (cons (x lazy-memo) (y lazy-memo)) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

(define (list-ref (items lazy-memo) (n lazy-memo))
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (add-lists (list1 lazy-memo) (list2 lazy-memo))
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1) (cdr list2))))))

(define fib (cons 0 (cons 1 (add-lists fib (cdr fib)))))
(print (list-ref fib 20))
```
