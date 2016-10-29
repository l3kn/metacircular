(include "eval.scm")

(if (null? (cdddr (argv)))
  (begin
    (print "Usage: csi -s main.scm input-file.scm")
    (exit)))

(define input (read-file (cadddr (argv))))

(define (eval-file file env)
  (if (not (null? file))
    (begin
      (eval_ (car file) env)
      (eval-file (cdr file) env))))

(eval-file input (setup-environment))
