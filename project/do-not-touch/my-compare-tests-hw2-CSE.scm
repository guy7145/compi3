(print-gensym #f)
                                        ; Change to your own location
(load "cse.scm")
(define my-parse-func cse)
(load "cse.so")
(define staff-parse-func cse)

(define a (lambda args 1))
(define b (lambda args 2))
(define c (lambda args 3))
(define foo (lambda args 3))
(define goo (lambda args 13))
(define f (lambda args 5))
(define g (lambda args (lambda args 6)))
(define h (lambda args 7))
(define x 5)
(define y 12)
(define z 18)

(define eval-input
  (lambda (cse input)
    (eval (cse input))
    ))

(define replace-gensym
  (lambda (exp-lst)
    (if (null? exp) '()
        (map (lambda (el)
               (cond
                ((gensym? el) (symbol->string el))
                ((not (pair? el)) el)
                ((list? el) (replace-gensym el))
                (else (append (replace-gensym (car exp-lst)) (replace-gensym (cdr exp-lst)))))) exp-lst))
    ))

(define verify-equality
  (lambda (input staff-res my-res)
    (and
     (equal? (car staff-res) (car my-res))
     (equal? (length (cadr staff-res)) (length (cadr my-res)))
     (equal? (length (cddr staff-res)) (length (cddr my-res)))
     (equal? (eval-input staff-parse-func input) (eval-input my-parse-func input)))
    ))

(define testVSstaff
  (lambda (input)
    (begin (display input)
      (let* ((my-res (begin (gensym-count 0) (replace-gensym (my-parse-func input))))
             (staff-res (begin (gensym-count 0) (replace-gensym (staff-parse-func input)))))
                                        ;(display (format "\n => ~s\n" my-res))
        (cond ((or (equal? staff-res my-res) (verify-equality input staff-res my-res))
               (display (format "\033[1;32m Success! ☺ \033[0m \n")) #t)
              (else
               (display (format "\033[1;31m Failed! ☹\033[0m ,\nExpected: ~s,\nActual:   ~s\n\n" staff-res my-res)) #f))
        ))))


(define runTests
  (lambda (tests-name lst)
    (newline)
    (display (format "\033[1m~s" tests-name))
    (display ":")
    (newline)
    (display "================\033[0m")
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

(define myTests
  (letrec ((build-test (lambda (op) `(- (,op 1 2) (,op 1 2))))
           (build-multiple-tests (lambda (ops)
                                   (if (null? ops)
                                       '()
                                       (cons (build-test (car ops)) (build-multiple-tests (cdr ops)))))))
    (append (build-multiple-tests '(and begin cond define do else if lambda let let* letrec or #\` quasiquote  #\, unquote ",@" unquote-splicing #\' quote set!))
            (list
             '(+ 2 3)
             '(or (or 1 2) (or 1 2))
             '(or (and 1 2) (and 1 2))
             '(append (quasiquote 1) (quasiquote 1))
             '(append (append '(1) '(2)) (append '(1) '(2)))
             '(begin `(1) `(1))
             '(begin `(1 2) `(1 2))
             '(begin `(1 a b) `(1 a b))
             '(begin `(1 ,a b) `(1 ,a b))
             '(begin `(1 a ,@b) `(1 a ,@b))
             '(list 1 2 3 4)
             '(list a a a b)
             '(list a ,a ,a b)
             '(list a ,@a ,@a ,@b c d)
             '(list (a) a (a) b)
             '(list ((f a)) ((f a)) ((f 1)) ((f 2)) ((f 2)) a (a) b)
             '(list ((f a)) ((f a)) ((f 1 2 3)) ((f 1 2 3)) ((f 1 2 3)) a (a) b)
             '(list '1 '2 '3 '1 '2 '3 'a 'a 'b 'b 'c 'c)
             '(list '('1 '2 '3) '('1 '2 '3) '(a a b b c c) '(a a b b c c))
             (build-test 'begin)))))

(display (format "\033[1mComp171 - CSE Tests\033[0m\n====================\n"))
(runAllTests
 (list
  (cons "My Tests" myTests)
  ))