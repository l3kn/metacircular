(define (list-ref (items lazy-memo) (n lazy-memo))
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (map (proc lazy-memo) (items lazy-memo))
  (if (null? items)
    '()
    (cons (proc (car items))
          (map proc (cdr items)))))

(define (scale-list (items lazy-memo) (factor lazy-memo))
  (map (lambda (x) (* x factor))
       items))

(define (add-lists (list1 lazy-memo) (list2 lazy-memo))
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1) (cdr list2))))))

(define ones (cons 1 ones))
(define integers
  (cons 1 (add-lists ones integers)))
