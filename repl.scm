(include "eval.scm")

(define input-prompt  "LE-Eval> ")
(define output-prompt "LE-Eval: ")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let* ((input (read))
         (output (actual-value input the-global-environment)))
    (announce-output output-prompt)
    (user-print output)
    (newline))
  (driver-loop))

(define (prompt-for-input string) (display string))
(define (announce-output string) (display string))

; Use a special procedure
; to avoid printing the environment part
; of a compound procedure (which might be very long)

(define (user-print object)
  (if (compound-procedure? object)
      (display
        (list 'compound-procedure
              (procedure-parameters object)
              (procedure-body object)
              '<procedure-env>))
      (display object)))

(define the-global-environment
  (setup-environment))

(driver-loop)
