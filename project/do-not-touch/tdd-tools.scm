;; styling tools
;;
(define display-green (lambda (msg) (display (format "\033[1;32m ~s \033[0m \n" msg))))
(define display-red (lambda (msg) (display (format "\033[1;31m ~s \033[0m \n" msg))))
(define display-colored (lambda (msg) (display (format "\033[1;33m ~s \033[0m \n" msg))))
(define display-colored-2 (lambda (msg) (display (format "\033[1;30m ~s \033[0m \n" msg))))
(define display-colored-title (lambda (msg) (display (format "\033[1;33m ~s: \033[0m \n" msg))))
(define display-colored-BIG (lambda (msg) (display (format "\033[1;36m ~s \033[0m \n" msg))))
(define display-colored-BIG-title (lambda (msg) (display (format "\033[1;36m ~s: \033[0m \n" msg))))
;; testing tools
;;
(define default-succ-cont (lambda () (display-green 'success!)))
(define default-fail-cont (lambda () (display-red 'failed!)))
(define default-maybe-cont (lambda () (display-colored 'maybe!)))

(define match-test
  (lambda (result expected success fail maybe)
    (cond ( ; success
           (equal? result expected) (success))
          ( ; fail
           (or (and (list? result) (not-list? expected))
               (and (not-list? result) (list? expected)))
           (fail))
          ( ; maybe
           (and (not-list? result) (not-list? expected)) 
           (maybe result expected)
           )
          (else ; next iteration
           (let ((succ-cont (lambda () (match-test (cdr result) (cdr expected) success fail maybe)))
                 (maybe-cont (lambda (g1 g2)
                               (display-colored-2 `(,g1 as ,g2))
                               (match-test (cdr result)
                                           (cdr expected) 
                                           maybe 
                                           fail 
                                           maybe))))
             (match-test
              (car result)
              (car expected)
              succ-cont
              fail
              maybe-cont))))))

(define ASSERT-MATCH
  (lambda (result expected)
    (match-test result
                expected
                default-succ-cont
                default-fail-cont
                default-maybe-cont)))

(define ASSERT-EQUAL
  (lambda (result expected)
      (cond ((equal? result expected) (begin (display-green 'passed!) #t))
	    (else (begin
		    (display-red 'failed!)
		    (display-colored-title 'expected:)
		    (display expected)
		    (newline)
		    (display-colored-title 'actual:)
		    (display result)
		    (newline)
                #f)))
      ))

(define ASSERT-EQUAL-ex
  (lambda (input parse f1 f2)
    (if (not (ASSERT-EQUAL (f1 input) (f2 input)))
        (begin (display-colored input) (display-colored-2 (parse input)) #f)
        #t)))
