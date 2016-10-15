; Exercise 4.16, 2

(define (scan-out-defines body)
  (display body)
  (let ((definitions (filter definition? body))
        (new-body (map (lambda (expr)
                         (if (definition? expr)
                           (list 'set!
                                 (definition-variable expr)
                                 (definition-value expr))
                           expr))
                       body)))
    (if (or (null? definitions) (null? (cdr definitions)))
      body
      (list (cons 'let
            (cons 
              (map (lambda (def)
                     (list (definition-variable def) (quote '*unassigned*)))
                   definitions)
              new-body))))))
