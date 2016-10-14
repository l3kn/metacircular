(include "eval.scm")

(define input-prompt  ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval_ input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

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
