(include "eval.scm")

(if (null? (cdddr (argv)))
  (begin
    (print "Usage: csi -s main.scm input-file.scm")
    (exit)))

(define the-global-environment (setup-environment))

(eval-file (read-file "stdlib.meta.scm") the-global-environment)
(eval-file
  (read-file (cadddr (argv)))
  the-global-environment)
