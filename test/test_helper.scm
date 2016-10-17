(define (assert-equal name expected result)
  (if (equal? expected result)
      (begin
        (display "Test \"")
        (display name)
        (display "\" passed\n"))
      (begin
        (display "Test \"")
        (display name)
        (display "\" failed: \n")
        (display "  expected ")
        (display expected)
        (newline)
        (display "  got      ")
        (display result)
        (newline))))

