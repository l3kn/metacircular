(define (delay-it exp env)
  ; (print "delaying " exp)
  (list 'thunk exp env))

(define (delay-it-memo exp env)
  ; (print "delaying " exp)
  (list 'memo-thunk exp env))

(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (memo-thunk? obj) (tagged-list? obj 'memo-thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         ; (print "forcing thunk " (thunk-exp obj))
         (actual-value
           (thunk-exp obj)
           (thunk-env obj)))
        ((memo-thunk? obj)
         ; (print "forcing thunk " (thunk-exp obj))
         (let ((result
                 (actual-value
                   (thunk-exp obj)
                   (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           ; replace exp with result
           (set-car! (cdr obj) result)
           ; forget unneeded env
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         ; (print "forcing eval thunk " (thunk-value obj))
         (thunk-value obj))
        ; (else (print "forcing obj ") obj)))
        (else obj)))

