(include "eval.scm")

(define input-prompt  "LE-Eval> ")
(define output-prompt "LE-Eval: ")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let* ((input (read))
         (output (actual-value input the-global-environment)))
    (announce-output output-prompt)
    (user-print output 10)
    (newline))
  (driver-loop))

(define (prompt-for-input string) (display string))
(define (announce-output string) (display string))

; Use a special procedure
; to avoid printing the environment part
; of a compound procedure (which might be very long)

(define (print-lazy-pair object limit)
  (let ((head (force-it (cadr object)))
        (tail (force-it (caddr object))))
    (user-print head (- limit 1))
    (display " . ")
    (user-print tail (- limit 1))))

(define (user-print object limit)
  (if (= limit 0)
    (display "...")
    (cond
      ((compound-procedure? object)
       (display
         (list 'compound-procedure
               (procedure-parameters object)
               (procedure-body object)
               '<procedure-env>)))
      ((lazy-pair? object)
       (display "(")
       (print-lazy-pair object limit)
       (display ")"))
      (else (display object)))))

(define the-global-environment (setup-environment))
(eval-file (read-file "stdlib.meta.scm") the-global-environment)

(driver-loop)
