                                        ; Change to your own location
(define code
  '(lamnda ()
    (begin
      (define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e))))))))

(load "compiler.scm")
(define my-parse-func parse)

(load "tagparser.so")
(define staff-parse-func parse)

(define try-catch
  (lambda (try-thunk catch-thunk)
    (guard (c (else (catch-thunk)))
     (try-thunk))))

(define testVSstaff
  (lambda (input)
    (let* ((my-res (try-catch (lambda () (my-parse-func input)) (lambda () "Exception thrown")))
           (staff-res (try-catch (lambda () (staff-parse-func input)) (lambda () "ERROR"))))
      (display (format "~s:" input))
                                        ;(display my-res)
      (cond ((equal? my-res staff-res)
             (display (format "\033[1;32m Success! ☺ \033[0m \n")) #t)
            (else
             (display (format "\033[1;31m Failed! ☹\033[0m ,\nExpected: ~s,\nActual:   ~s\n\n" staff-res my-res)) #f))
      )))

(define runTests
  (lambda (tests-name lst)
    (newline)
    (display tests-name)
    (display ":")
    (newline)
    (display "=============")
    (newline)
    (let ((results (map testVSstaff lst)))
      (newline)
      (cond ((andmap (lambda (exp) (equal? exp #t)) results)
             (display (format "\033[1;32m~s Tests: SUCCESS! ☺ \033[0m\n \n" tests-name)) #t)
            (else
             (display (format "\033[1;31m~s Tests: FAILED! ☹ \033[0m\n \n" tests-name)) #f)))
    ))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
      (cond ((andmap (lambda (exp) (equal? exp #t)) results)
             (display "\033[1;32m !!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n"))
            (else (display "\033[1;31m #####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n")))
      (newline))
    ))

(define MyTests
  (list
   ''()
   ''(1 2 3)
   '(1 2 3)
   '(lambda (x y z . e) (e (f x y z123)))
   '(cond)
   '(define (foo x y))
   '(quasiquote a)
   '(quasiquote (a))
   '(quasiquote)
   '(void)
   'qq
   '(qq)
   'quasiquote
   'seq
   '(seq)
   '(seq (seq 1))
   '(seq (seq))
   code
   ))

(runAllTests
 (list
  (cons "MyTests" MyTests)
  ))


















